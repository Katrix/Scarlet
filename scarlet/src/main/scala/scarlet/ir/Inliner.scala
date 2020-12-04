package scarlet.ir

import cats.data.State
import scarlet.graph.CFG
import scarlet.ir.SIR.{Expr, TempVar}
import perspective._

import scala.annotation.tailrec
import scala.collection.immutable.{Queue, TreeMap}

object Inliner {

  private type Code = TreeMap[Long, Vector[SIR]]

  private case class QueueEntry(
      tempVar: TempVar,
      expr: Expr[_],
      codeUpdater: Code => Code
  )

  /**
    * Dequeues elements from the queue until the predicate returns true.
    *
    * @return Returns None together with the empty queue if the queue became
    *         empty before the predicate was fulfilled. Otherwise returns Some
    *         with the queue element that fulfilled the predicate,
    *         together with the modified queue.
    */
  @tailrec
  private def dequeueUntil[A](queue: Queue[A])(f: A => Boolean): (Queue[A], Option[A]) = {
    if (queue.isEmpty) (queue, None)
    else {
      val (elem, newQueue) = queue.dequeue
      if (f(elem)) (newQueue, Some(elem))
      else dequeueUntil(newQueue)(f)
    }
  }

  private case class PropagationStep(queue: Queue[QueueEntry], resultCode: Code) {
    //noinspection MutatorLikeMethodIsParameterless
    def removeSideEffectsExprs: PropagationStep =
      copy(queue = queue.reverseIterator.takeWhile(e => !canExprCauseExceptions(e.expr)).to(Queue).reverse)
  }

  private def canExprCauseExceptions(e: Expr[_]): Boolean = e match {
    case Expr.BinaryExpr(_, _, SIR.BinaryOp.Div, _) | Expr.BinaryExpr(_, _, SIR.BinaryOp.Rem, _) |
        Expr.IsInstanceOf(_, _) | Expr.UninitializedRef(_, _) | Expr.ArrayLength(_) | Expr.GetArray(_, _) |
        Expr.GetField(_, _) | Expr.GetStatic(_) | Expr.New(_, _) | Expr.Call(_, _, _, _, _, _) | Expr.NewArray(_, _) |
        Expr.NewMultiArray(_, _) | Expr.Cast(_, _) =>
      true
    case _ => false
  }

  def removeFakesFromSIR(block: CFG.SIRBlock.SIRCodeBasicBlock): CFG.SIRBlock.SIRCodeBasicBlock = {

    /*
    Example 1:
    a := 10 % 2
    b := 5 / x
    c := f(a, b)

    Into
    nop
    nop
    c := f(10 % 2, 5 / x)

    Example 2:
    a := 10 % 2
    b := 5 / x
    c := f(b, a)

    Into
    a := 10 % 2
    nop
    c := f(5 / x, a)
     */

    if(block.code.sizeIs > 10) {
      val r = identity(block.code)
    }

    val tempVarUses = block.code.foldLeft(Map.empty[TempVar, Int]) {
      case (topAcc, (_, codes)) =>
        codes.foldLeft(topAcc) { (acc, code) =>
          code.monoFoldLeft(acc)(
            new FunctionK[Tuple2K[Const[Map[TempVar, Int], *], Expr, *], Const[Map[TempVar, Int], *]] {
              override def apply[A](fa: Tuple2K[Const[Map[TempVar, Int], *], Expr, A]): Const[Map[TempVar, Int], A] = {
                val acc  = fa._1
                val expr = fa._2

                expr match {
                  case Expr.GetFakeLocal(tempVar, _) => acc.updatedWith(tempVar)(o => Some(o.fold(1)(_ + 1)))
                  case _                             => acc
                }
              }
            }
          )
        }
    }

    type PropagateState[A] = State[PropagationStep, A]

    lazy val exprTraverser: FunctionK[Expr, Compose2[PropagateState, Expr, *]] =
      new FunctionK[Expr, Compose2[PropagateState, Expr, *]] {
        override def apply[Z](expr: Expr[Z]): PropagateState[Expr[Z]] = {
          State.get[PropagationStep].flatMap { step =>
            if(step.queue.isEmpty) State.pure(expr)
            else {
              expr match {
                case Expr.GetFakeLocal(tempVar, _) =>
                  val hasSingleUse = tempVarUses.get(tempVar).contains(1)
                  if (hasSingleUse) {
                    val (resultQueue, fakeLocalExprOpt) = dequeueUntil(step.queue)(_.tempVar.index == tempVar.index)

                    fakeLocalExprOpt match {
                      case Some(QueueEntry(_, fakeLocalExpr, codeUpdater)) =>
                        State
                          .set(
                            step.copy(
                              queue = resultQueue,
                              resultCode = codeUpdater(step.resultCode)
                            )
                          )
                          .map(_ => fakeLocalExpr.asInstanceOf[Expr[Z]])

                      case None =>
                        // Not sure if this is reachable, but if it is,
                        // something else exhausted the queue before we got here,
                        // so we use the original step. Not quite sure if that's
                        // correct, but let's hope so
                        State.pure(expr)
                    }
                  } else {
                    State.pure(expr)
                  }
                case _ =>
                  val baseRun = expr.monoTraverseDeep[PropagateState](exprTraverser)

                  if (canExprCauseExceptions(expr))
                    baseRun.modify(_.removeSideEffectsExprs)
                  else
                    baseRun
              }
            }
          }
        }
      }

    val resultStep = block.code.foldLeft(PropagationStep(Queue.empty, block.code)) {
      case (topStep, (idx, codes)) =>
        codes.zipWithIndex.foldLeft(topStep) {
          case (startStep, (ir, innerIdx)) =>

            def substituteExpr[A](e: Expr[A]): (PropagationStep, Expr[A]) =
              exprTraverser(e).run(startStep).value

            def trySubstitute: PropagationStep = {
              val (newStep, newIR) = ir.monoTraverseDeep[PropagateState](exprTraverser).run(startStep).value

              newStep.copy(
                resultCode = newStep.resultCode.updatedWith(idx) {
                  case Some(vector) => Some(vector.updated(innerIdx, newIR))
                  case None         => Some(Vector(newIR))
                }
              )
            }

            def addTempvar[A](tempVar: TempVar, e: Expr[A]): PropagationStep = {
              val (newStep, newE) = substituteExpr(e)
              newStep.copy(
                queue = newStep.queue.enqueue(
                  QueueEntry(
                    tempVar,
                    newE,
                    codeUpdater = _.updatedWith(idx) {
                      case Some(vec) if vec.sizeIs > 1 => Some(vec.patch(innerIdx, Nil, 1))
                      case _                           => Some(Vector(SIR.Nop))
                    }
                  )
                )
              )
            }

            def trySubstituteAndRemoveSideEffects: PropagationStep =
              trySubstitute.removeSideEffectsExprs

            ir match {
              case SIR.SetFakeLocal(tempVar, e) =>
                tempVarUses.get(tempVar) match {
                  case Some(uses) if uses > 1 => trySubstitute //Treat it like normal
                  case Some(1)                => addTempvar(tempVar, e)
                  case Some(0)                =>
                    //Def here to avoid existential warning by capturing the existential
                    def capture[A](expr: Expr[A]): PropagationStep = {
                      //We replace the code with execute expr
                      val (newStep, newE) = substituteExpr(expr)

                      newStep.copy(resultCode = newStep.resultCode.updatedWith(idx) {
                        case Some(value) => Some(value.updated(innerIdx, SIR.ExecuteExpr(newE)))
                        case None        => Some(Vector(SIR.ExecuteExpr(newE)))
                      })
                    }

                    capture(e)

                  case None => trySubstitute // Should be unreachable, but just in case, we treat it like normal
                }
              case SIR.MaybeInit(_) | SIR.SetArray(_, _, _) | SIR.SetField(_, _, _) | SIR.SetStatic(_, _) |
                  SIR.CallSuper(_, _) | SIR.MonitorEnter(_) | SIR.MonitorExit(_) =>
                trySubstituteAndRemoveSideEffects
              case _ => trySubstitute
            }
        }
    }

    block.copy(code = resultStep.resultCode)
  }
}
