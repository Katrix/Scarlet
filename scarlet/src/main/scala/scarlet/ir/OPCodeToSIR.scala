package scarlet.ir

import scala.language.implicitConversions

import scala.collection.immutable.LongMap

import scarlet.classfile.denormalized.Descriptor
import scarlet.classfile.denormalized.opcodes.OPCode
import scarlet.classfile.denormalized.opcodes.OPCode.{Type => OPType}
import scarlet.graph.OPCodeCFG
import scarlet.ir.SIR.{Expr, Type => IRType}

/**
  * A converter from denormalized opcodes to [[SIR]].
  *
  * Uses the algorithm from the paper linked to in the comment of the SIR trait.
  */
object OPCodeToSIR {

  type Stack = List[Expr[_]]

  type CodeWithStack = LongMap[(Stack, OPCode, Vector[SIR], Stack)]

  def convert(
      code: LongMap[OPCode],
      opcodecfg: OPCodeCFG
  ): Either[(String, CodeWithStack), CodeWithStack] = {
    import cats.syntax.all._

    val jumpTargets: Set[Long] = opcodecfg.jumpTargets

    def tempAssign(targetPoints: Set[Long], stack: Stack): Vector[SIR] =
      targetPoints.flatMap { tp =>
        stack.zipWithIndex.collect {
          case (e: Expr[_], idx) =>
            SIR.SetStackLocal(idx, tp, e)
        }
      }.toVector

    def newStackJump(jumpTarget: Long, code: CodeWithStack): Stack =
      code.values
        .flatMap(_._3)
        .collect {
          case SIR.SetStackLocal(index, `jumpTarget`, e) => Expr.GetStackLocal(index, jumpTarget, e.tpe)
        }
        .toList

    code.toVector
      .foldLeft(
        (List.empty[Expr[_]], LongMap.empty[(Stack, OPCode, Vector[SIR], Stack)], 0)
          .asRight[(String, CodeWithStack)]
      ) {
        case (Right((inStack, irMap, tempVarCount)), (pc, opcode)) =>
          val succ = opcodecfg.cfg.get(pc).diSuccessors.map(_.value)

          val res = for {
            newInStack <- {
              if (jumpTargets.contains(pc)) {
                if (!true /* ???*/ ) Left(???)
                else Right(newStackJump(pc, irMap))
              } else Right(inStack)
            }
            t <- convertOne(opcode, pc, newInStack, tempVarCount)
            (irCode, outStack, newTempVarCount) = t
            newIr                               = tempAssign(succ.intersect(jumpTargets), outStack) ++ irCode
            _ <- Either.cond(
              outStack == Nil || succ.forall(pc < _),
              (),
              s"Non empty stack backwards jump at $pc with succ $succ"
            )
            nextStack = if (succ.contains(pc + 1) && !jumpTargets.contains(pc + 1)) outStack else Nil
          } yield (nextStack, irMap.updated(pc, (inStack, opcode, newIr, outStack)), newTempVarCount)

          res.leftMap(_ -> irMap)
        case (Left(e), (_, _)) => Left(e)
      }
      .map(_._2)
  }

  private def convertOne(
      opCode: OPCode,
      pc: Long,
      stack: Stack,
      tempVarCount: Int
  ): Either[String, (Seq[SIR], Stack, Int)] = {

    implicit def opToIrType[A](tpe: OPType[A]): IRType.Aux[A] = tpe match {
      case OPType.Boolean => IRType.Boolean.asInstanceOf[IRType.Aux[A]]
      case OPType.Byte    => IRType.Byte.asInstanceOf[IRType.Aux[A]]
      case OPType.Short   => IRType.Short.asInstanceOf[IRType.Aux[A]]
      case OPType.Int     => IRType.Int.asInstanceOf[IRType.Aux[A]]
      case OPType.Long    => IRType.Long.asInstanceOf[IRType.Aux[A]]
      case OPType.Float   => IRType.Float.asInstanceOf[IRType.Aux[A]]
      case OPType.Double  => IRType.Double.asInstanceOf[IRType.Aux[A]]
      case OPType.Char    => IRType.Char.asInstanceOf[IRType.Aux[A]]
      case OPType.String  => IRType.String.asInstanceOf[IRType.Aux[A]]
      case OPType.Class   => IRType.Class.asInstanceOf[IRType.Aux[A]]
      case OPType.Ref     => IRType.AnyRef.asInstanceOf[IRType.Aux[A]]
    }

    def nopSimple(newStack: Stack): Either[String, (Seq[SIR], Stack, Int)] =
      Right((Seq(SIR.Nop), newStack, tempVarCount))
    def nop(newStack: Either[String, Stack]): Either[String, (Seq[SIR], Stack, Int)] =
      newStack.map(s => (Seq(SIR.Nop), s, tempVarCount))
    def seqSimple(res: (SIR, Stack)): Either[String, (Seq[SIR], Stack, Int)] =
      Right((Seq(res._1), res._2, tempVarCount))
    def seq(res: Either[String, (SIR, Stack)]): Either[String, (Seq[SIR], Stack, Int)] =
      res.map(t => (Seq(t._1), t._2, tempVarCount))

    def isCat2Type(expr: Expr[_]): Boolean = IRType.Category2.isSupertypeOf(expr.tpe)

    def stack1[B](tpe: IRType, stackToUse: Stack = stack)(
        use: (Expr[tpe.A], Stack) => B
    ): Either[String, B] = stackToUse match {
      case (h: Expr[tpe.A @unchecked]) :: t if tpe.isSupertypeOf(h.tpe) => Right(use(h, t))
      case (h: Expr[_]) :: _ =>
        Left(s"Wanted ${tpe.describe} in stack, but found ${h.tpe.describe} instead at $pc")
      case _ :: _ => Left(s"Wanted ${tpe.describe} in stack, but found symbolic expr instead at $pc")
      case Nil    => Left(s"Wanted one value in the stack, but it was empty at $pc")
    }

    def stack11[C](tpe1: IRType, tpe2: IRType, stackToUse: Stack = stack)(
        use: (Expr[tpe2.A], Expr[tpe1.A], Stack) => C
    ): Either[String, C] =
      stack1(tpe1, stackToUse = stackToUse)((e1, r1) => stack1(tpe2, stackToUse = r1)((e2, r2) => use(e2, e1, r2)))
        .flatMap(identity)

    def stack2[B](tpe: IRType, stackToUse: Stack = stack)(
        use: (Expr[tpe.A], Expr[tpe.A], Stack) => B
    ): Either[String, B] =
      stack11(tpe, tpe, stackToUse = stackToUse)(use)

    def stack1Add(tpe: IRType)(use: Expr[tpe.A] => Expr[_]): Either[String, Stack] =
      stack1(tpe)(use(_) :: _)

    def stack11Add(tpe1: IRType, tpe2: IRType)(
        use: (Expr[tpe2.A], Expr[tpe1.A]) => Expr[_]
    ): Either[String, Stack] = stack11(tpe1, tpe2)(use(_, _) :: _)

    def stack2Add(tpe: IRType)(use: (Expr[tpe.A], Expr[tpe.A]) => Expr[_]): Either[String, Stack] =
      stack2(tpe)(use(_, _) :: _)

    def condIntExpr(cond: OPCode.IntIfCond, e1: Expr[Int], e2: Expr[Int]): Expr[Boolean] = cond match {
      case OPCode.IntIfCond.EQ => Expr.Eq(e1, e2)
      case OPCode.IntIfCond.NE => Expr.Not(Expr.Eq(e1, e2))
      case OPCode.IntIfCond.LT => Expr.LT(e1, e2)
      case OPCode.IntIfCond.GE => Expr.GE(e1, e2)
      case OPCode.IntIfCond.GT => Expr.GT(e1, e2)
      case OPCode.IntIfCond.LE => Expr.LE(e1, e2)
    }

    def condRefCmpExpr(cond: OPCode.RefIfCmpCond, e1: Expr[AnyRef], e2: Expr[AnyRef]): Expr[Boolean] = cond match {
      case OPCode.RefIfCmpCond.EQ => Expr.Eq(e1, e2)
      case OPCode.RefIfCmpCond.NE => Expr.Not(Expr.Eq(e1, e2))
    }

    def condRefExpr(cond: OPCode.RefIfCond, e: Expr[AnyRef]): Expr[Boolean] = cond match {
      case OPCode.RefIfCond.IsNull    => Expr.Eq(e, Expr.Null)
      case OPCode.RefIfCond.IsNotNull => Expr.Not(Expr.Eq(e, Expr.Null))
    }

    //I don't think we need to store the old values for stuff as pointed out in the paper. Let's see what happens
    opCode match {
      case OPCode.Nop => nopSimple(stack)

      case OPCode.PushNull         => nopSimple(Expr.Null :: stack)
      case OPCode.Push(tpe, value) => nopSimple(Expr.ConstTpe(tpe, value) :: stack)

      case OPCode.VarLoad(tpe, index) => nopSimple(Expr.GetLocal(index, opToIrType(tpe)) :: stack)
      case OPCode.ArrayLoad(tpe) =>
        val irTpe = opToIrType(tpe)
        seq(
          stack11(IRType.Array(irTpe), OPType.Int) {
            case (e2, e1: Expr[Array[irTpe.A] @unchecked], r) =>
              (SIR.NotNull(e1): SIR, Expr.GetArray[irTpe.A](e1, e2, irTpe) :: r)
          }
        )
      case OPCode.VarStore(tpe, index) => seq(stack1(tpe)((e1, r) => (SIR.SetLocal(index, e1), r)))
      case OPCode.ArrayStore(tpe) =>
        val irTpe = opToIrType(tpe)
        stack11(irTpe, IRType.Int) {
          case (e2, e1: Expr[irTpe.A], r1) =>
            stack1(IRType.Array(irTpe), stackToUse = r1) {
              case (e3: Expr[Array[irTpe.A] @unchecked], r2) =>
                (Seq(SIR.NotNull(e3), SIR.SetArray[irTpe.A](e3, e2, e1)), r2, tempVarCount)
            }
        }.flatMap(identity)
      case OPCode.Pop1 => nop(stack1(IRType.Category1)((_, r) => r))
      case OPCode.Pop2 =>
        nop(
          stack1(IRType.Any) { (e1, r1) =>
            if (isCat2Type(e1)) Right(r1)
            else stack1(IRType.Category1, stackToUse = r1)((_, r2) => r2)
          }.flatMap(identity)
        )
      case OPCode.Dup   => nop(stack1(IRType.Category1)((e, r) => e :: e :: r))
      case OPCode.DupX1 => nop(stack2(IRType.Category1)((e1, e2, r) => e1 :: e2 :: e1 :: r))
      case OPCode.DupX2 =>
        nop(
          stack2(IRType.Any) { (e1, e2, r1) =>
            if (isCat2Type(e2)) Right(e1 :: e2 :: e1 :: r1)
            else stack1(IRType.Category1, stackToUse = r1)((e3, r2) => e1 :: e2 :: e3 :: e1 :: r2)
          }.flatMap(identity)
        )
      case OPCode.Dup2 =>
        nop(
          stack1(IRType.Any) { (e1, r1) =>
            if (isCat2Type(e1)) Right(e1 :: e1 :: r1)
            else stack1(IRType.Category1, stackToUse = r1)((e2, r2) => e1 :: e2 :: e1 :: e2 :: r2)
          }.flatMap(identity)
        )
      case OPCode.Dup2X1 =>
        nop(
          stack1(IRType.Any) { (e1, r1) =>
            if (isCat2Type(e1))
              stack1(IRType.Category1, stackToUse = r1)((e2, r2) => e1 :: e2 :: e1 :: r2)
            else stack2(IRType.Category1, stackToUse = r1)((e2, e3, r2) => e1 :: e2 :: e3 :: e1 :: e2 :: r2)
          }.flatMap(identity)
        )
      case OPCode.Dup2X2 =>
        nop(
          stack1(IRType.Any) { (e1, r1) =>
            if (isCat2Type(e1)) {
              stack1(IRType.Any, stackToUse = r1) { (e2, r2) =>
                if (isCat2Type(e2)) Right(e1 :: e2 :: e1 :: r2)
                else stack1(IRType.Category1, stackToUse = r2)((e3, r3) => e1 :: e2 :: e3 :: e1 :: r3)
              }.flatMap(identity)
            } else {
              stack1(IRType.Category1, stackToUse = r1) { (e2, r2) =>
                stack1(IRType.Any, stackToUse = r2) { (e3, r3) =>
                  if (isCat2Type(e3)) Right(e1 :: e2 :: e3 :: e1 :: e2 :: r3)
                  else stack1(IRType.Category1, stackToUse = r3)((e4, r4) => e1 :: e2 :: e3 :: e4 :: e1 :: e2 :: r4)
                }.flatMap(identity)
              }.flatMap(identity)
            }
          }.flatMap(identity)
        )

      case OPCode.Swap => nop(stack2(IRType.Category1)((e1, e2, r) => e2 :: e1 :: r))

      case OPCode.Add(tpe)           => nop(stack2Add(tpe)(Expr.Add(_, _)))
      case OPCode.Sub(tpe)           => nop(stack2Add(tpe)(Expr.Sub(_, _)))
      case OPCode.Mult(tpe)          => nop(stack2Add(tpe)(Expr.Mult(_, _)))
      case OPCode.Div(tpe)           => seq(stack2(tpe)((e1, e2, r) => (SIR.NotZero(e2), Expr.Div(e1, e2) :: r)))
      case OPCode.Rem(tpe)           => seq(stack2(tpe)((e1, e2, r) => (SIR.NotZero(e2), Expr.Rem(e1, e2) :: r)))
      case OPCode.Neg(tpe)           => nop(stack1Add(tpe)(Expr.Neg(_)))
      case OPCode.ShiftLeft(tpe)     => nop(stack11Add(tpe, IRType.Int)((e2, e1) => Expr.ShiftLeft(e1, e2)))
      case OPCode.ShiftRight(tpe)    => nop(stack11Add(tpe, IRType.Int)((e2, e1) => Expr.ShiftRight(e1, e2)))
      case OPCode.LogShiftRight(tpe) => nop(stack11Add(tpe, IRType.Int)((e2, e1) => Expr.LogShiftRight(e1, e2)))
      case OPCode.And(tpe)           => nop(stack2Add(tpe)(Expr.And(_, _)))
      case OPCode.Or(tpe)            => nop(stack2Add(tpe)(Expr.Or(_, _)))
      case OPCode.Xor(tpe)           => nop(stack2Add(tpe)(Expr.Xor(_, _)))

      case OPCode.IntVarIncr(index, amount) =>
        seqSimple(
          (SIR.SetLocal(index, Expr.Add(Expr.ConstTpe(IRType.Int, amount), Expr.GetLocal(index, IRType.Int))), stack)
        )

      case OPCode.Conversion(from, to) => nop(stack1Add(from)(Expr.Convert(_, opToIrType(to))))

      case OPCode.Compare(tpe, nanBehavior) => nop(stack2Add(tpe)(Expr.Compare(_, _, nanBehavior)))

      case OPCode.IntIfZero(cond, branchPC) =>
        seq(
          stack1(IRType.Int)((e, r) => (SIR.If(condIntExpr(cond, e, Expr.ConstTpe(IRType.Int, 0)), branchPC), r))
        )
      case OPCode.IntIfCmp(cond, branchPC) =>
        seq(stack2(IRType.Int)((e1, e2, r) => (SIR.If(condIntExpr(cond, e1, e2), branchPC), r)))
      case OPCode.RefIf(cond, branchPC) =>
        seq(stack1(IRType.AnyRef)((e, r) => (SIR.If(condRefExpr(cond, e), branchPC), r)))
      case OPCode.RefIfCmp(cond, branchPC) =>
        seq(stack2(IRType.AnyRef)((e1, e2, r) => (SIR.If(condRefCmpExpr(cond, e1, e2), branchPC), r)))
      case OPCode.Goto(branchPC) => seqSimple((SIR.Goto(branchPC), stack))
      case OPCode.Switch(defaultPC, pairs) =>
        seq(stack1(IRType.Int)((e, r) => (SIR.Switch(e, defaultPC, pairs), r)))

      case OPCode.Return(Some(tpe)) => seq(stack1(tpe)((e, r) => (SIR.Return(Some(e)), r)))
      case OPCode.Return(None)      => seqSimple((SIR.Return(None), stack))

      case OPCode.GetStatic(fieldRefInfo) =>
        seqSimple((SIR.MaybeInit(fieldRefInfo.clazz), Expr.GetStatic(fieldRefInfo) :: stack))
      case OPCode.PutStatic(fieldRefInfo) =>
        stack1(IRType.Any)(
          (e, r) => (Seq(SIR.MaybeInit(fieldRefInfo.clazz), SIR.SetStatic(fieldRefInfo, e)), r, tempVarCount)
        )
      case OPCode.GetField(fieldRefInfo) =>
        seq(stack1(IRType.AnyRef)((e, r) => (SIR.NotNull(e), Expr.GetField(e, fieldRefInfo) :: r)))
      case OPCode.PutField(fieldRefInfo) =>
        stack2(IRType.AnyRef)((ep, e, r) => (Seq(SIR.NotNull(e), SIR.SetField(e, ep, fieldRefInfo)), r, tempVarCount))

      case OPCode.Invoke(methodInfo, tpe) =>
        val descriptor = methodInfo.nameAndType.descriptor
        val mdesc = descriptor match {
          case meth: Descriptor.MethodDescriptor => Right(meth)
          case _                                 => Left(s"Found unexpected descriptor type ${descriptor.getClass.getSimpleName} in call at $pc")
        }

        val irTpe = tpe match {
          case OPCode.InvokeType.Virtual   => SIR.CallType.Virtual
          case OPCode.InvokeType.Special   => SIR.CallType.Special
          case OPCode.InvokeType.Static    => SIR.CallType.Static
          case OPCode.InvokeType.Interface => SIR.CallType.Interface
        }

        mdesc.flatMap { desc =>
          val paramCount      = desc.paramTypes.size
          val callParamsCount = if (irTpe == SIR.CallType.Static) paramCount else paramCount + 1

          if (stack.lengthCompare(callParamsCount) >= 0) {
            val (params, newStack) = stack.splitAt(callParamsCount)

            val methodName = methodInfo.nameAndType.name
            if (irTpe == SIR.CallType.Special && methodName == "<init>") {
              val paramsInit = params.init
              val paramsLast = params.last
              val paramsVec  = paramsInit.toVector

              if (paramsInit.lengthCompare(paramsVec.length) == 0) {
                paramsLast match {
                  case uninit @ Expr.UninitializedRef(atAddress, classInfo) =>
                    val codes = Seq(SIR.New(tempVarCount, classInfo, paramsVec))

                    Right(
                      (
                        codes,
                        newStack.map(_.substitute(uninit, Expr.GetFakeLocal(tempVarCount, IRType.Ref(classInfo)))),
                        tempVarCount + 1
                      )
                    )
                  case expr: Expr[_] =>
                    val codes = Seq(SIR.NotNull(expr), SIR.CallSuper(expr, paramsVec))

                    Right((codes, newStack, tempVarCount))
                }
              } else Left(s"Found uninitialized reference in object creation call at $pc")
            } else {
              val paramsVec = params.toVector.reverse

              if (params.lengthCompare(paramsVec.length) == 0) {
                val codes = if (irTpe == SIR.CallType.Static) {
                  Seq(SIR.Call(tempVarCount, irTpe, methodInfo.clazz, methodName, desc, None, paramsVec))
                } else {
                  val e = paramsVec.head
                  Seq(
                    SIR.NotNull(e),
                    SIR.Call(tempVarCount, irTpe, methodInfo.clazz, methodName, desc, Some(e), paramsVec.tail)
                  )
                }

                if (desc.returnType == Descriptor.VoidType) Right((codes, newStack, tempVarCount + 1))
                else
                  Right(
                    (
                      codes,
                      Expr.GetFakeLocal(tempVarCount, IRType.Unknown: IRType.Aux[_]) :: newStack,
                      tempVarCount + 1
                    )
                  )
              } else Left(s"Found uninitialized reference in method call at $pc")
            }
          } else Left(s"Not enough stack params for call at $pc")
        }

      case OPCode.INVOKEDYNAMIC(indexByte1, indexByte2) => ???

      case OPCode.New(classInfo) =>
        seqSimple((SIR.MaybeInit(classInfo), Expr.UninitializedRef(pc, classInfo) :: stack))

      case OPCode.NewArray(tpe) =>
        stack1(IRType.Int) { (e, r) =>
          (
            Seq(SIR.NotNegative(e), SIR.NewArray(tempVarCount, e, opToIrType(tpe))),
            Expr.GetFakeLocal(tempVarCount, IRType.Array(opToIrType(tpe))) :: r,
            tempVarCount + 1
          )
        }
      case OPCode.RefNewArray(classInfo) =>
        stack1(IRType.Int) { (e, r) =>
          (
            Seq(SIR.NotNegative(e), SIR.NewArray(tempVarCount, e, IRType.Ref(classInfo))),
            Expr.GetFakeLocal(tempVarCount, IRType.Array(IRType.Ref(classInfo))) :: r,
            tempVarCount + 1
          )
        }
      case OPCode.MultiRefNewArray(classInfo, dimensions) =>
        if (stack.lengthCompare(dimensions) >= 0) {
          val (arraySizes, newStack) = stack.splitAt(dimensions)

          val sizesExpr = arraySizes.collect {
            case e: Expr[Int @unchecked] if e.tpe == IRType.Int => e
          }.toVector

          if (arraySizes.lengthCompare(sizesExpr.length) == 0) {
            val dimExpr  = Expr.ConstTpe(IRType.Int, dimensions)
            val dimTests = Seq(SIR.NotNegative(dimExpr), SIR.NotZero(dimExpr)) ++ sizesExpr.map(s => SIR.NotNegative(s))

            val tpe = (0 until dimensions)
              .foldLeft(IRType.Ref(classInfo): IRType)(
                (tpe, _) => IRType.Array(tpe.asInstanceOf[IRType.Aux[_]]): IRType
              )
              .asInstanceOf[IRType.Aux[Array[_]]]

            Right(
              (
                dimTests :+ SIR.NewMultiArray(tempVarCount, tpe, sizesExpr),
                Expr.GetFakeLocal(tempVarCount, tpe) :: newStack,
                tempVarCount + 1
              )
            )
          } else Left(s"Found invalid stack type in multi array initialization at $pc")
        } else Left(s"Not enough stack params for multi array initialization at $pc")
      case OPCode.ArrayLength => nop(stack1Add(IRType.AnyArray)(Expr.ArrayLength))

      case OPCode.RefThrow              => nopSimple(stack) //TODO
      case OPCode.Cast(classInfo)       => nop(stack1Add(IRType.AnyRef)(Expr.Cast(_, classInfo)))
      case OPCode.InstanceOf(classInfo) => nop(stack1Add(IRType.AnyRef)(Expr.IsInstanceOf(_, classInfo)))
      case OPCode.MonitorEnter          => seq(stack1(IRType.AnyRef)((e, r) => (SIR.MonitorEnter(e), r)))
      case OPCode.MonitorExit           => seq(stack1(IRType.AnyRef)((e, r) => (SIR.MonitorExit(e), r)))
    }
  }
}
