package scarlet.ir

import scarlet.classfile.denormalized.Descriptor
import scarlet.classfile.denormalized.opcodes.OPCode
import scarlet.classfile.denormalized.opcodes.OPCode.{MethodInfo, Type => OPType}
import scarlet.graph.CFG
import scarlet.ir.SIR.{Expr, TempVar, Type => IRType}

import scala.annotation.tailrec
import scala.collection.immutable.{LongMap, TreeMap}
import scala.language.implicitConversions

/**
  * A converter from denormalized opcodes to [[SIR]].
  *
  * Uses the algorithm from the paper linked to in the comment of the SIR trait.
  */
object OPCodeToSIR {

  val WeakTypingIntTypes    = true
  val intTypes: Set[IRType] = Set(IRType.Int, IRType.Short, IRType.Char, IRType.Byte, IRType.Boolean)

  type Stack         = List[Expr[_]]
  type CodeWithStack = LongMap[StackFrame]

  case class StackFrame(before: Stack, op: OPCode, code: SIR, after: Stack)
  case class BlockStep[A](
      accNodes: Map[A, CFG.SIRBlock],
      predecessorsInfo: Map[A, StackInfo],
      tempVar: TempVar
  )
  case class StackStep(stack: Stack, irMap: CodeWithStack, tempVar: TempVar)
  case class CodeStep(code: SIR, stack: Stack, tempVar: TempVar)

  case class StackInfo(types: List[IRType])
  object StackInfo {
    val Empty: StackInfo = StackInfo(Nil)

    //TODO: Handle exceptions
    // From the JVMS:
    // In the special case of control transfer to an exception handler,
    // the operand stack is set to contain a single object of the exception type
    // indicated by the exception handler information. There must be sufficient
    // room on the operand stack for this single value,
    // as if an instruction had pushed it.
    def stackFromInfo(info: StackInfo, jumpTarget: Long): Stack = info.types.zipWithIndex.map {
      case (tpe, idx) =>
        Expr.GetStackLocal(idx, jumpTarget, tpe.asInstanceOf[SIR.Type.Aux[_]])
    }

    def fromStack(stack: Stack): StackInfo = StackInfo(stack.map(_.tpe))
  }

  def convert(code: CFG[CFG.OPCodeBasicBlock]): CFG[CFG.SIRBlock] = {
    import cats.syntax.all._
    import scalax.collection.GraphPredef._
    import scalax.collection.edge.LBase.LEdgeImplicits
    import scalax.collection.immutable.Graph
    object TupleLabelImplicit extends LEdgeImplicits[(code.graph.NodeT, code.graph.NodeT)]
    import TupleLabelImplicit._

    val scc = CFG.stronglyConnectedComponents(code.graph)
    val sortedNodes: Vector[code.graph.NodeT] = if (scc.nodes.sizeIs > 1) {
      scc.topologicalSortByComponent.toVector
        .flatMap(
          _.getOrElse(sys.error("impossible"))
            .withLayerOrdering(scc.NodeOrdering { (n1, n2) =>
              val l1 = n1.value.nodes.view.map(_.value.leader).minOption
              val l2 = n2.value.nodes.view.map(_.value.leader).minOption

              l1.compare(l2)
            })
            .toVector
        )
        .distinct
        .flatMap { sccNode =>
          val component = sccNode.value
          if (component.nodes.size == 1) {
            Seq(component.nodes.head.asInstanceOf[code.graph.NodeT])
          } else {
            val subgraph     = Graph.from(component.nodes.map(_.toOuter), component.edges.map(_.toOuter))
            val earliestNode = sccNode.incoming.minByOption(_.label._2.value.leader)
            val startNode    = earliestNode.map(_.label._2).map(n => subgraph.get(n.value)).getOrElse(subgraph.nodes.head)

            subgraph.innerNodeTraverser(startNode).toVector.map(subgraphNode => code.graph.get(subgraphNode.value))
          }
        }
    } else {
      code.graph.innerNodeTraverser(code.graph.get(code.start)).toVector
    }

    def dumpStackSingle(node: code.graph.NodeT, stack: Stack): Vector[SIR] = {
      val leaderPc = node.value.leader
      stack.zipWithIndex.map {
        case (e, idx) =>
          SIR.SetStackLocal(idx, leaderPc, e)
      }.toVector
    }

    def dumpStackMultiple(nodes: Set[code.graph.NodeT], stack: Stack, tempVar: TempVar): (Vector[SIR], TempVar) = {
      val (saveFake, tempVarOut) = stack.foldLeft((Vector.empty[SIR], tempVar)) {
        case ((acc, newTempVar), stackValue) => (acc :+ SIR.SetFakeLocal(newTempVar, stackValue), newTempVar.inc)
      }

      val res = nodes.toVector.flatMap { node =>
        val leaderPc = node.value.leader
        stack.zipWithIndex.map {
          case (e, idx) =>
            SIR.SetStackLocal(idx, leaderPc, Expr.GetFakeLocal(new TempVar(tempVar.index + idx), e.tpe))
        }.toVector
      }

      (saveFake ++ res, tempVarOut)
    }

    val BlockStep(sirBlocks, _, _) = sortedNodes.foldLeft(
      BlockStep(Map.empty[code.graph.NodeT, CFG.SIRBlock], Map.empty[code.graph.NodeT, StackInfo], new TempVar(0))
    ) {
      case (BlockStep(accNodes, predecessorsInfo, blockTempVar), node) =>
        val predecessors = node.diPredecessors
        val successors   = node.diSuccessors
        val opCodeBlock  = node.value
        val leader       = opCodeBlock.leader

        val predecessorInfo = predecessors.collectFirst[StackInfo](predecessorsInfo).getOrElse(StackInfo.Empty)
        val startStack      = StackInfo.stackFromInfo(predecessorInfo, leader)

        val eitherResult = opCodeBlock.code.foldLeft(
          StackStep(startStack, LongMap.empty[StackFrame], blockTempVar)
            .asRight[(Long, String, OPCode, CodeWithStack, TempVar)]
        ) {
          case (Right(StackStep(inStack, irMap, codeTempVar)), (pc, opcode)) =>
            convertOne(opcode, pc, inStack, codeTempVar) match {
              case Right(CodeStep(irCode, outStack, newTempVar)) =>
                Right(
                  StackStep(
                    outStack,
                    irMap.updated(pc, StackFrame(inStack, opcode, irCode, outStack)),
                    newTempVar
                  )
                )
              case Left(e) => Left((pc, e, opcode, irMap, codeTempVar))
            }
          case (Left(e), (_, _)) => Left(e)
        }

        eitherResult match {
          case Right(StackStep(stack, irMap, outTempVar)) =>
            val baseCode = irMap.transform((_, f) => Vector(f.code))

            val (code, finalTempVar) = if (successors.nonEmpty) {
              val (dumpCode, outTempVarAfterDump) =
                if (successors.sizeIs > 1)
                  dumpStackMultiple(successors, stack, outTempVar)
                else
                  (dumpStackSingle(successors.head, stack), outTempVar)

              //At this point it should only contain one element lists
              val lastPc           = baseCode.lastKey
              val lastCode         = baseCode(lastPc).head
              val lastCodeWithDump = dumpCode :+ lastCode

              (baseCode.updated(lastPc, lastCodeWithDump), outTempVarAfterDump)
            } else {
              (baseCode, outTempVar)
            }

            @tailrec
            def runWhileChangesN[A](n: Int, f: A => A)(oldA: A): A =
              if (n == 0) oldA
              else {
                val newA = f(oldA)
                if (oldA != newA) runWhileChangesN(n - 1, f)(newA)
                else newA
              }

            val sirBlock                 = CFG.SIRBlock.SIRCodeBasicBlock(leader, code.to(TreeMap))
            val inlinedConstantsSirBlock = SIRPostProcess.inlineTrueFakeConstants(sirBlock)

            //TODO: Make amount of times to run the inliner configurable
            val (simplifiedSirBlock, inlinerTempvar) =
              runWhileChangesN[(CFG.SIRBlock.SIRCodeBasicBlock, TempVar)](
                5,
                t => SIRPostProcess.removeFakesFromSIR(t._1, t._2)
              )((inlinedConstantsSirBlock, finalTempVar))
            val weakTypingFixed = SIRPostProcess.fixSimpleWeakTyping(simplifiedSirBlock)

            BlockStep(
              accNodes.updated(node, weakTypingFixed),
              predecessorsInfo.updated(node, StackInfo.fromStack(stack)),
              inlinerTempvar
            )

          case Left((pc, error, op, code, outTempVar)) =>
            BlockStep(
              accNodes.updated(node, CFG.SIRBlock.SIRErrorBlock(pc, error, op, code)),
              predecessorsInfo,
              outTempVar
            )
        }
    }

    val newNodes = sirBlocks.values
    val newEdges = code.graph.edges.map(edge => sirBlocks(edge.from) ~> sirBlocks(edge.to))

    val newGraph = Graph.from(newNodes, newEdges)
    CFG(newGraph, newGraph.get(sirBlocks(code.graph.get(code.start))))
  }

  private def convertOne(
      opCode: OPCode,
      pc: Long,
      stack: Stack,
      tempVar: TempVar
  ): Either[String, CodeStep] = {

    implicit def opToIrType(tpe: OPType): IRType.Aux[tpe.A] = tpe match {
      case OPType.Boolean => IRType.Boolean.asInstanceOf[IRType.Aux[tpe.A]]
      case OPType.Byte    => IRType.Byte.asInstanceOf[IRType.Aux[tpe.A]]
      case OPType.Short   => IRType.Short.asInstanceOf[IRType.Aux[tpe.A]]
      case OPType.Int     => IRType.Int.asInstanceOf[IRType.Aux[tpe.A]]
      case OPType.Long    => IRType.Long.asInstanceOf[IRType.Aux[tpe.A]]
      case OPType.Float   => IRType.Float.asInstanceOf[IRType.Aux[tpe.A]]
      case OPType.Double  => IRType.Double.asInstanceOf[IRType.Aux[tpe.A]]
      case OPType.Char    => IRType.Char.asInstanceOf[IRType.Aux[tpe.A]]
      case OPType.String  => IRType.String.asInstanceOf[IRType.Aux[tpe.A]]
      case OPType.Class   => IRType.Class.asInstanceOf[IRType.Aux[tpe.A]]
      case OPType.Ref     => IRType.AnyRef.asInstanceOf[IRType.Aux[tpe.A]]
    }

    /** A simple conversion that just modifies the stack */
    def nopSimple(newStack: Stack): Either[String, CodeStep] =
      Right(CodeStep(SIR.Nop, newStack, tempVar))

    /** A simple conversion that just modifies the stack, but can fail */
    def nop(newStack: Either[String, Stack]): Either[String, CodeStep] =
      newStack.flatMap(nopSimple)

    /** A simple conversion that results in a new IR code, and a new stack */
    def irSimple(ir: SIR, newStack: Stack): Either[String, CodeStep] =
      Right(CodeStep(ir, newStack, tempVar))

    /** A conversion that can fail that results in a new IR code, and a new stack */
    def ir(res: Either[String, (SIR, Stack)]): Either[String, CodeStep] =
      res.map(t => CodeStep(t._1, t._2, tempVar))

    /** Is the expression type of type 2 */
    def isCat2Type(expr: Expr[_]): Boolean = IRType.Category2.isSupertypeOf(expr.tpe)

    /** Checks if the type of an expr matches a needed type. Might modify the expr slightly to make stuff work */
    def typeMatches[A](expr: Expr[_], wantedType: IRType.Aux[A]): Option[Expr[A]] = {
      val exprType    = expr.tpe
      val isSupertype = wantedType.isSupertypeOf(exprType)
      if (!isSupertype && WeakTypingIntTypes && intTypes.contains(exprType) && intTypes.contains(wantedType)) {
        Some(Expr.Convert(expr, wantedType))
      } else if (isSupertype) {
        Some(expr.asInstanceOf[Expr[A]])
      } else None
    }

    /** Use one value on the stack */
    def stack1[B](tpe: IRType, stackToUse: Stack = stack)(
        use: (Expr[tpe.A], Stack) => B
    ): Either[String, B] = {
      val preciseType: IRType.Aux[tpe.A] = tpe
      stackToUse match {
        case (h: Expr[_]) :: t if typeMatches(h, preciseType).isDefined =>
          Right(use(typeMatches(h, preciseType).get, t))
        case (h: Expr[_]) :: _ =>
          Left(s"Wanted ${tpe.describe} in stack, but found ${h.tpe.describe} instead at $pc")
        case Nil => Left(s"Wanted one value in the stack, but it was empty at $pc")
      }
    }

    /** Use two values of different types on the stack */
    def stack11[C](tpe1: IRType, tpe2: IRType, stackToUse: Stack = stack)(
        use: (Expr[tpe2.A], Expr[tpe1.A], Stack) => C
    ): Either[String, C] =
      stack1(tpe1, stackToUse = stackToUse)((e1, r1) =>
        stack1(tpe2, stackToUse = r1)((e2, r2) => use(e2, e1, r2))
      ).flatten

    /** Use two values of the same type on the stack */
    def stack2[B](tpe: IRType, stackToUse: Stack = stack)(
        use: (Expr[tpe.A], Expr[tpe.A], Stack) => B
    ): Either[String, B] =
      stack11(tpe, tpe, stackToUse = stackToUse)(use)

    /** Use three values of different types on the stack */
    def stack111[C](tpe1: IRType, tpe2: IRType, tpe3: IRType, stackToUse: Stack = stack)(
        use: (Expr[tpe3.A], Expr[tpe2.A], Expr[tpe1.A], Stack) => C
    ): Either[String, C] =
      stack11(tpe1, tpe2, stackToUse = stackToUse)((e2, e1, r1) =>
        stack1(tpe3, stackToUse = r1)((e3, r2) => use(e3, e2, e1, r2))
      ).flatten

    /** Transform one value on the stack */
    def stack1Add(tpe: IRType)(use: Expr[tpe.A] => Expr[_]): Either[String, Stack] =
      stack1(tpe)(use(_) :: _)

    /** Transform two values of different types on the stack */
    def stack11Add(tpe1: IRType, tpe2: IRType)(
        use: (Expr[tpe2.A], Expr[tpe1.A]) => Expr[_]
    ): Either[String, Stack] = stack11(tpe1, tpe2)(use(_, _) :: _)

    /** Transform two values of the same type on the stack */
    def stack2Add(tpe: IRType)(use: (Expr[tpe.A], Expr[tpe.A]) => Expr[_]): Either[String, Stack] =
      stack2(tpe)(use(_, _) :: _)

    def condIntExpr(cond: OPCode.IntIfCond, e1: Expr[Int], e2: Expr[Int]): Expr[Boolean] = {
      val opType = cond match {
        case OPCode.IntIfCond.EQ => SIR.BinaryOp.Equal
        case OPCode.IntIfCond.NE => SIR.BinaryOp.NotEqual
        case OPCode.IntIfCond.LT => SIR.BinaryOp.LT
        case OPCode.IntIfCond.GE => SIR.BinaryOp.GE
        case OPCode.IntIfCond.GT => SIR.BinaryOp.GT
        case OPCode.IntIfCond.LE => SIR.BinaryOp.LE
      }
      Expr.BinaryExpr(e1, e2, opType, IRType.Boolean)
    }

    def condRefCmpExpr(cond: OPCode.RefIfCmpCond, e1: Expr[AnyRef], e2: Expr[AnyRef]): Expr[Boolean] = {
      val opType = cond match {
        case OPCode.RefIfCmpCond.EQ => SIR.BinaryOp.Equal
        case OPCode.RefIfCmpCond.NE => SIR.BinaryOp.NotEqual
      }
      Expr.BinaryExpr(e1, e2, opType, IRType.Boolean)
    }

    def condRefExpr(cond: OPCode.RefIfCond, e: Expr[AnyRef]): Expr[Boolean] =
      cond match {
        case OPCode.RefIfCond.IsNull    => Expr.BinaryExpr(e, Expr.Null, SIR.BinaryOp.Equal, IRType.Boolean)
        case OPCode.RefIfCond.IsNotNull => Expr.BinaryExpr(e, Expr.Null, SIR.BinaryOp.NotEqual, IRType.Boolean)
      }

    //I don't think we need to store the old values for stuff as pointed out in the paper. Let's see what happens
    opCode match {
      case OPCode.Nop => nopSimple(stack)

      case OPCode.PushNull         => nopSimple(Expr.Null :: stack)
      case OPCode.Push(tpe, value) => nopSimple(Expr.ConstTpe(tpe, value) :: stack)

      case OPCode.VarLoad(tpe, index) =>
        Right(
          CodeStep(
            SIR.SetFakeLocal(tempVar, Expr.GetLocal(index)),
            Expr.GetFakeLocal(tempVar, tpe) :: stack,
            tempVar.inc
          )
        )
      case OPCode.ArrayLoad(tpe) =>
        val irTpe = opToIrType(tpe)
        stack11(IRType.Array(irTpe), IRType.Int) {
          case (e2, e1, r) =>
            CodeStep(
              SIR.SetFakeLocal(tempVar, Expr.GetArray(e1, e2)),
              Expr.GetFakeLocal(tempVar, irTpe) :: r,
              tempVar.inc
            )
        }
      case OPCode.VarStore(tpe, index) =>
        stack1(opToIrType(tpe).asInstanceOf[IRType.Aux[AnyRef]]) {
          case (_: Expr.UninitializedRef, _) => Left("Can't assign uninitialized reference to variable")
          case (e1, r) =>
            Right(CodeStep(SIR.SetLocal(index, e1), r, tempVar))
        }.flatten
      case OPCode.ArrayStore(tpe) =>
        val irTpe = opToIrType(tpe)
        ir(
          stack111(
            irTpe.asInstanceOf[IRType.Aux[AnyRef]],
            IRType.Int,
            IRType.Array(irTpe.asInstanceOf[IRType.Aux[AnyRef]])
          ) {
            case (_, _, _: Expr.UninitializedRef, _) => Left("Can't assign uninitialized reference to array")
            case (e3, e2, e1, r2) =>
              Right((SIR.SetArray(e3, e2, e1), r2))
          }.flatten
        )
      case OPCode.Pop1 => nop(stack1(IRType.Category1)((_, r) => r))
      case OPCode.Pop2 =>
        nop(
          stack1(IRType.Any) { (e1, r1) =>
            if (isCat2Type(e1)) Right(r1)
            else stack1(IRType.Category1, stackToUse = r1)((_, r2) => r2)
          }.flatten
        )
      case OPCode.Dup   => nop(stack1(IRType.Category1)((e, r) => e :: e :: r))
      case OPCode.DupX1 => nop(stack2(IRType.Category1)((e1, e2, r) => e1 :: e2 :: e1 :: r))
      case OPCode.DupX2 =>
        nop(
          stack2(IRType.Any) { (e1, e2, r1) =>
            if (isCat2Type(e2)) Right(e1 :: e2 :: e1 :: r1)
            else stack1(IRType.Category1, stackToUse = r1)((e3, r2) => e1 :: e2 :: e3 :: e1 :: r2)
          }.flatten
        )
      case OPCode.Dup2 =>
        nop(
          stack1(IRType.Any) { (e1, r1) =>
            if (isCat2Type(e1)) Right(e1 :: e1 :: r1)
            else stack1(IRType.Category1, stackToUse = r1)((e2, r2) => e1 :: e2 :: e1 :: e2 :: r2)
          }.flatten
        )
      case OPCode.Dup2X1 =>
        nop(
          stack1(IRType.Any) { (e1, r1) =>
            if (isCat2Type(e1))
              stack1(IRType.Category1, stackToUse = r1)((e2, r2) => e1 :: e2 :: e1 :: r2)
            else stack2(IRType.Category1, stackToUse = r1)((e2, e3, r2) => e1 :: e2 :: e3 :: e1 :: e2 :: r2)
          }.flatten
        )
      case OPCode.Dup2X2 =>
        nop(
          stack1(IRType.Any) { (e1, r1) =>
            if (isCat2Type(e1)) {
              stack1(IRType.Any, stackToUse = r1) { (e2, r2) =>
                if (isCat2Type(e2)) Right(e1 :: e2 :: e1 :: r2)
                else stack1(IRType.Category1, stackToUse = r2)((e3, r3) => e1 :: e2 :: e3 :: e1 :: r3)
              }.flatten
            } else {
              stack1(IRType.Category1, stackToUse = r1) { (e2, r2) =>
                stack1(IRType.Any, stackToUse = r2) { (e3, r3) =>
                  if (isCat2Type(e3)) Right(e1 :: e2 :: e3 :: e1 :: e2 :: r3)
                  else stack1(IRType.Category1, stackToUse = r3)((e4, r4) => e1 :: e2 :: e3 :: e4 :: e1 :: e2 :: r4)
                }.flatten
              }.flatten
            }
          }.flatten
        )

      case OPCode.Swap => nop(stack2(IRType.Category1)((e1, e2, r) => e1 :: e2 :: r))

      case OPCode.Add(tpe)  => nop(stack2Add(tpe)(Expr.BinaryExpr(_, _, SIR.BinaryOp.Add, tpe)))
      case OPCode.Sub(tpe)  => nop(stack2Add(tpe)(Expr.BinaryExpr(_, _, SIR.BinaryOp.Sub, tpe)))
      case OPCode.Mult(tpe) => nop(stack2Add(tpe)(Expr.BinaryExpr(_, _, SIR.BinaryOp.Mul, tpe)))
      case OPCode.Div(tpe) =>
        stack2(tpe)((e1, e2, r) =>
          CodeStep(
            SIR.SetFakeLocal(tempVar, Expr.BinaryExpr(e1, e2, SIR.BinaryOp.Div, tpe)),
            Expr.GetFakeLocal(tempVar, tpe) :: r,
            tempVar.inc
          )
        )
      case OPCode.Rem(tpe) =>
        stack2(tpe)((e1, e2, r) =>
          CodeStep(
            SIR.SetFakeLocal(tempVar, Expr.BinaryExpr(e1, e2, SIR.BinaryOp.Rem, tpe)),
            Expr.GetFakeLocal(tempVar, tpe) :: r,
            tempVar.inc
          )
        )
      case OPCode.Neg(tpe) => nop(stack1Add(tpe)(Expr.UnaryExpr(_, SIR.UnaryOp.Neg, tpe)))
      case OPCode.ShiftLeft(tpe) =>
        nop(stack11Add(tpe, IRType.Int)((e2, e1) => Expr.BinaryExpr(e1, e2, SIR.BinaryOp.ShiftLeft, tpe)))
      case OPCode.ShiftRight(tpe) =>
        nop(stack11Add(tpe, IRType.Int)((e2, e1) => Expr.BinaryExpr(e1, e2, SIR.BinaryOp.ShiftRight, tpe)))
      case OPCode.LogShiftRight(tpe) =>
        nop(stack11Add(tpe, IRType.Int)((e2, e1) => Expr.BinaryExpr(e1, e2, SIR.BinaryOp.LogShiftRight, tpe)))
      case OPCode.And(tpe) => nop(stack2Add(tpe)(Expr.BinaryExpr(_, _, SIR.BinaryOp.BitAnd, tpe)))
      case OPCode.Or(tpe)  => nop(stack2Add(tpe)(Expr.BinaryExpr(_, _, SIR.BinaryOp.BitOr, tpe)))
      case OPCode.Xor(tpe) => nop(stack2Add(tpe)(Expr.BinaryExpr(_, _, SIR.BinaryOp.Xor, tpe)))

      case OPCode.IntVarIncr(index, amount) => irSimple(SIR.IntVarIncr(index, amount), stack)

      case OPCode.Conversion(from, to) => nop(stack1Add(from)(Expr.Convert(_, opToIrType(to))))

      case OPCode.Compare(tpe, nanBehavior) =>
        nop(stack2Add(tpe)(Expr.BinaryExpr(_, _, SIR.BinaryOp.Compare(nanBehavior), tpe)))

      case OPCode.IntIfZero(cond, branchPC) =>
        ir(
          stack1(IRType.Int)((e, r) => (SIR.If(condIntExpr(cond, e, Expr.ConstTpe(IRType.Int, 0)), branchPC), r))
        )
      case OPCode.IntIfCmp(cond, branchPC) =>
        ir(stack2(IRType.Int)((e1, e2, r) => (SIR.If(condIntExpr(cond, e1, e2), branchPC), r)))
      case OPCode.RefIf(cond, branchPC) =>
        ir(stack1(IRType.AnyRef)((e, r) => (SIR.If(condRefExpr(cond, e), branchPC), r)))
      case OPCode.RefIfCmp(cond, branchPC) =>
        ir(stack2(IRType.AnyRef)((e1, e2, r) => (SIR.If(condRefCmpExpr(cond, e1, e2), branchPC), r)))
      case OPCode.Goto(branchPC) => irSimple(SIR.Goto(branchPC), stack)
      case OPCode.Switch(defaultPC, pairs) =>
        ir(stack1(IRType.Int)((e, r) => (SIR.Switch(e, defaultPC, pairs), r)))

      case OPCode.Return(Some(tpe)) => ir(stack1(tpe)((e, r) => (SIR.Return(Some(e)), r)))
      case OPCode.Return(None)      => irSimple(SIR.Return(None), stack)

      case OPCode.GetStatic(fieldRefInfo) =>
        Right(
          CodeStep(
            SIR.SetFakeLocal(tempVar, Expr.GetStatic(fieldRefInfo)),
            Expr.GetFakeLocal(tempVar, IRType.fromDescriptor(fieldRefInfo.nameAndType.descriptor)) :: stack,
            tempVar.inc
          )
        )
      case OPCode.PutStatic(fieldRefInfo) =>
        ir(stack1(IRType.Any)((e, r) => (SIR.SetStatic(fieldRefInfo, e), r)))
      case OPCode.GetField(fieldRefInfo) =>
        stack1(IRType.AnyRef)((e, r) =>
          CodeStep(
            SIR.SetFakeLocal(tempVar, Expr.GetField(e, fieldRefInfo)),
            Expr.GetFakeLocal(tempVar, IRType.fromDescriptor(fieldRefInfo.nameAndType.descriptor)) :: r,
            tempVar.inc
          )
        )
      case OPCode.PutField(fieldRefInfo) =>
        ir(stack2(IRType.AnyRef)((ep, e, r) => (SIR.SetField(e, ep, fieldRefInfo), r)))

      case invoke @ OPCode.Invoke(_, _) =>
        handleInvoke(invoke, stack, tempVar, pc)

      case OPCode.INVOKEDYNAMIC(indexByte1, indexByte2) => ???

      case OPCode.New(classInfo) =>
        irSimple(SIR.MaybeInit(classInfo), Expr.UninitializedRef(pc, classInfo) :: stack)

      case OPCode.NewArray(tpe) =>
        stack1(IRType.Int) { (e, r) =>
          CodeStep(
            SIR.SetFakeLocal(tempVar, Expr.NewArray(e, opToIrType(tpe))),
            Expr.GetFakeLocal(tempVar, IRType.Array(opToIrType(tpe))) :: r,
            tempVar.inc
          )
        }
      case OPCode.RefNewArray(classInfo) =>
        stack1(IRType.Int) { (e, r) =>
          CodeStep(
            SIR.SetFakeLocal(tempVar, Expr.NewArray(e, IRType.Ref(classInfo))),
            Expr.GetFakeLocal(tempVar, IRType.Array(IRType.Ref(classInfo))) :: r,
            tempVar.inc
          )
        }
      case OPCode.MultiRefNewArray(classInfo, dimensions) =>
        if (stack.lengthIs >= dimensions) {
          val (arraySizes, newStack) = stack.splitAt(dimensions)

          val sizesExpr = arraySizes.collect {
            case e: Expr[Int @unchecked] if e.tpe == IRType.Int => e
          }.toVector

          if (arraySizes.lengthIs == sizesExpr.length) {
            val tpe =
              (0 until dimensions)
                .foldLeft(IRType.Ref(classInfo): IRType)((tpe, _) =>
                  IRType.Array(tpe.asInstanceOf[IRType.Aux[_]]): IRType
                )
                .asInstanceOf[IRType.Aux[Array[_]]]

            Right(
              CodeStep(
                SIR.SetFakeLocal(tempVar, Expr.NewMultiArray(tpe, sizesExpr)),
                Expr.GetFakeLocal(tempVar, tpe) :: newStack,
                tempVar.inc
              )
            )
          } else Left(s"Found invalid stack type in multi array initialization at $pc")
        } else Left(s"Not enough stack params for multi array initialization at $pc")
      case OPCode.ArrayLength =>
        nop(stack1Add(IRType.AnyArray)(arr => Expr.ArrayLength(arr)))

      case OPCode.RefThrow => ir(stack1(IRType.AnyRef)((e, r) => (SIR.Throw(e), r)))

      case OPCode.Cast(classInfo) =>
        stack1(IRType.AnyRef)((e, r) =>
          CodeStep(
            SIR.SetFakeLocal(tempVar, Expr.Cast(e, IRType.Ref(classInfo))),
            Expr.GetFakeLocal(tempVar, IRType.Ref(classInfo)) :: r,
            tempVar.inc
          )
        )
      case OPCode.InstanceOf(classInfo) => nop(stack1Add(IRType.AnyRef)(Expr.IsInstanceOf(_, classInfo)))
      case OPCode.MonitorEnter          => ir(stack1(IRType.AnyRef)((e, r) => (SIR.MonitorEnter(e), r)))
      case OPCode.MonitorExit           => ir(stack1(IRType.AnyRef)((e, r) => (SIR.MonitorExit(e), r)))
    }
  }

  def handleInvoke(invoke: OPCode.Invoke, stack: Stack, tempVar: TempVar, pc: Long): Either[String, CodeStep] = {
    val nameAndType = invoke.methodRefInfo.nameAndType

    val descriptor = nameAndType.descriptor
    val mdesc = descriptor match {
      case meth: Descriptor.MethodDescriptor => Right(meth)
      case _                                 => Left(s"Found unexpected descriptor type ${descriptor.getClass.getSimpleName} in call at $pc")
    }

    val callType = invoke.invokeType match {
      case OPCode.InvokeType.Virtual   => SIR.CallType.Virtual
      case OPCode.InvokeType.Special   => SIR.CallType.Special
      case OPCode.InvokeType.Static    => SIR.CallType.Static
      case OPCode.InvokeType.Interface => SIR.CallType.Interface
    }

    mdesc.flatMap { desc =>
      val paramCount      = desc.paramTypes.size
      val callParamsCount = if (callType == SIR.CallType.Static) paramCount else paramCount + 1

      if (stack.lengthIs >= callParamsCount) {
        val (params, newStack) = stack.splitAt(callParamsCount)

        if (callType == SIR.CallType.Special && nameAndType.name == "<init>")
          handleInit(params, newStack, tempVar, pc)
        else
          handleMethodCall(params, newStack, invoke.methodRefInfo, desc, callType, tempVar, pc)
      } else Left(s"Not enough stack params for call at $pc")
    }
  }

  def handleInit(params: Stack, stack: Stack, tempVar: TempVar, pc: Long): Either[String, CodeStep] = {
    val paramsInit = params.init
    val paramsLast = params.last
    val paramsVec = paramsInit.toVector.collect {
      case e: Expr[_] if !e.isInstanceOf[Expr.UninitializedRef] => e
    }

    if (paramsInit.lengthIs == paramsVec.length) {
      paramsLast match {
        case uninit @ Expr.UninitializedRef(atAddress, classInfo) =>
          Right(
            CodeStep(
              SIR.SetFakeLocal(tempVar, Expr.New(classInfo, paramsVec)),
              stack.map(_.substitute(uninit, Expr.GetFakeLocal(tempVar, IRType.Ref(classInfo)))),
              tempVar.inc
            )
          )
        case expr =>
          Right(CodeStep(SIR.CallSuper(expr, paramsVec), stack, tempVar))
      }
    } else Left(s"Found uninitialized reference in object creation call at $pc")
  }

  def handleMethodCall(
      params: Stack,
      stack: Stack,
      methodInfo: MethodInfo,
      desc: Descriptor.MethodDescriptor,
      callType: SIR.CallType,
      tempVar: TempVar,
      pc: Long
  ): Either[String, CodeStep] = {
    val paramsVec = params.toVector.reverse.collect {
      case e: Expr[_] if !e.isInstanceOf[Expr.UninitializedRef] => e
    }
    val methodName = methodInfo.nameAndType.name

    if (params.lengthIs == paramsVec.length) {
      val call = SIR.SetFakeLocal(
        tempVar,
        Expr.Call(
          callType,
          methodInfo.clazz,
          methodName,
          desc,
          if (callType == SIR.CallType.Static) None else Some(paramsVec.head),
          if (callType == SIR.CallType.Static) paramsVec else paramsVec.tail
        )
      )

      desc.returnType match {
        case Descriptor.VoidType => Right(CodeStep(call, stack, tempVar.inc))
        case returnType: Descriptor.FieldType =>
          Right(
            CodeStep(
              call,
              Expr.GetFakeLocal(tempVar, IRType.fromDescriptor(returnType)) :: stack,
              tempVar.inc
            )
          )
      }
    } else Left(s"Found uninitialized reference in method call at $pc")
  }
}
