package scarlet.ir

import cats.arrow.FunctionK
import scarlet.classfile.denormalized.ConstantPoolEntry.FieldRefInfo
import scarlet.classfile.denormalized.Descriptor
import scarlet.classfile.denormalized.opcodes.OPCode
import scarlet.classfile.denormalized.opcodes.OPCode.{MethodInfo, Type => OPType}
import scarlet.graph.OPCodeCFG
import scarlet.ir.SIR.{Expr, Type => IRType}

import scala.collection.immutable.LongMap
import scala.language.implicitConversions

/**
  * A converter from denormalized opcodes to [[SIR]].
  *
  * Uses the algorithm from the paper linked to in the comment of the SIR trait.
  */
object OPCodeToSIR {

  type Stack         = List[Expr[_]]
  type CodeWithStack = LongMap[StackFrame]

  case class StackFrame(before: Stack, op: OPCode, code: Vector[SIR], after: Stack)
  case class StackStep(stack: Stack, irMap: CodeWithStack, tempVarCount: Int)
  case class Conversion(code: Seq[SIR], stack: Stack, tempVarCount: Int)
  case class SemanticSave(saveIr: Seq[SIR], newTempVarCount: Int, saveSubstitutedStack: Stack)

  def convert(
      code: LongMap[OPCode],
      opcodecfg: OPCodeCFG
  ): Either[(String, CodeWithStack), CodeWithStack] = {
    import cats.syntax.all._

    val jumpTargets: Set[Long] = opcodecfg.jumpTargets

    def dumpStackForJumps(targetPoints: Set[Long], stack: Stack): Vector[SIR] =
      targetPoints.flatMap { tp =>
        stack.zipWithIndex.collect {
          case (e: Expr[_], idx) =>
            SIR.SetStackLocal(idx, tp, e)
        }
      }.toVector

    def newStackJump(jumpTarget: Long, code: CodeWithStack): Stack =
      code.values
        .flatMap(_.code)
        .collect {
          case SIR.SetStackLocal(index, `jumpTarget`, e) => Expr.GetStackLocal(index, jumpTarget, e.tpe)
        }
        .toList

    code.toVector
      .foldLeft(
        StackStep(List.empty[Expr[_]], LongMap.empty[StackFrame], 0).asRight[(String, CodeWithStack)]
      ) {
        case (Right(StackStep(inStack, irMap, tempVarCount)), (pc, opcode)) =>
          val successors = opcodecfg.cfg.get(pc).diSuccessors.map(_.value)

          val res = for {
            stackWithJumpLocals <- {
              if (jumpTargets.contains(pc)) {
                if (!true /* ???*/ ) Left(???)
                else Right(newStackJump(pc, irMap))
              } else Right(inStack)
            }
            t <- convertOne(opcode, pc, stackWithJumpLocals, tempVarCount)
            Conversion(irCode, outStack, newTempVarCount) = t
            jumpSuccessors                                = successors.intersect(jumpTargets)
            newIr                                         = dumpStackForJumps(jumpSuccessors, outStack) ++ irCode
            _ <- Either.cond(
              outStack == Nil || successors.forall(pc < _),
              (),
              s"Non empty stack backwards jump at $pc with succ $successors"
            )
            nextStack = if (successors.contains(pc + 1) && !jumpTargets.contains(pc + 1)) outStack else Nil
          } yield StackStep(nextStack, irMap.updated(pc, StackFrame(inStack, opcode, newIr, outStack)), newTempVarCount)

          res.leftMap(_ -> irMap)
        case (Left(e), (_, _)) => Left(e)
      }
      .map(_.irMap)
  }

  private def convertOne(
      opCode: OPCode,
      pc: Long,
      stack: Stack,
      tempVarCount: Int
  ): Either[String, Conversion] = {

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
    def nopSimple(newStack: Stack): Either[String, Conversion] =
      Right(Conversion(Seq(SIR.Nop), newStack, tempVarCount))

    /** A simple conversion that just modifies the stack, but can fail */
    def nop(newStack: Either[String, Stack]): Either[String, Conversion] =
      newStack.flatMap(nopSimple)

    /** A simple conversion that results in a new IR code, and a new stack */
    def seqSimple(ir: SIR, newStack: Stack): Either[String, Conversion] =
      Right(Conversion(Seq(ir), newStack, tempVarCount))

    /** A conversion that can fail that results in a new IR code, and a new stack */
    def seq(res: Either[String, (SIR, Stack)]): Either[String, Conversion] =
      res.map(t => Conversion(Seq(t._1), t._2, tempVarCount))

    /** Is the expression type of type 2 */
    def isCat2Type(expr: Expr[_]): Boolean = IRType.Category2.isSupertypeOf(expr.tpe)

    /** Use one value on the stack */
    def stack1[B](tpe: IRType, stackToUse: Stack = stack)(
        use: (Expr[tpe.A], Stack) => B
    ): Either[String, B] = stackToUse match {
      case (h: Expr[tpe.A @unchecked]) :: t if tpe.isSupertypeOf(h.tpe) => Right(use(h, t))
      case (h: Expr[_]) :: _ =>
        Left(s"Wanted ${tpe.describe} in stack, but found ${h.tpe.describe} instead at $pc")
      case Nil => Left(s"Wanted one value in the stack, but it was empty at $pc")
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
        stack11(IRType.Array(irTpe), IRType.Int) {
          case (e2, e1, r) =>
            Conversion(
              Seq(SIR.NotNull(e1), SIR.ArrayIndexValid(e1.asInstanceOf[Expr[scala.Array[_]]], e2)),
              Expr.GetArray[irTpe.A](e1, e2, irTpe) :: r,
              tempVarCount
            )
        }
      case OPCode.VarStore(tpe, index) =>
        stack1(tpe) {
          case (_: Expr.UninitializedRef, _) => Left("Can't assign uninitialized reference to variable")
          case (e1, r) =>
            val hasGetLocal = r.collectFirst {
              case get @ Expr.GetLocal(`index`, _) => get
            }

            hasGetLocal match {
              case Some(existingGetLocal) =>
                Right(
                  Conversion(
                    Seq(SIR.SetFakeLocal(tempVarCount, existingGetLocal), SIR.SetLocal(index, e1)),
                    r.map(_.substitute(existingGetLocal, Expr.GetFakeLocal(tempVarCount, existingGetLocal.tpe))),
                    tempVarCount + 1
                  )
                )
              case None => Right(Conversion(Seq(SIR.SetLocal(index, e1)), r, tempVarCount))
            }
        }.flatten
      case OPCode.ArrayStore(tpe) =>
        val irTpe = opToIrType(tpe)
        stack111(irTpe, IRType.Int, IRType.Array(irTpe)) {
          case (_, _, _: Expr.UninitializedRef, _) => Left("Can't assign uninitialized reference to array")
          case (e3, e2, e1, r2)                    =>
            //TODO: Figure out what to save
            //Treat it roughly the same as a field as it's a reference
            Right(Conversion(Seq(SIR.NotNull(e3), SIR.SetArray(e3, e2, e1)), r2, tempVarCount))
        }.flatten
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
          SIR.SetLocal(index, Expr.Add(Expr.ConstTpe(IRType.Int, amount), Expr.GetLocal(index, IRType.Int))),
          stack
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
      case OPCode.Goto(branchPC) => seqSimple(SIR.Goto(branchPC), stack)
      case OPCode.Switch(defaultPC, pairs) =>
        seq(stack1(IRType.Int)((e, r) => (SIR.Switch(e, defaultPC, pairs), r)))

      case OPCode.Return(Some(tpe)) => seq(stack1(tpe)((e, r) => (SIR.Return(Some(e)), r)))
      case OPCode.Return(None)      => seqSimple(SIR.Return(None), stack)

      case OPCode.GetStatic(fieldRefInfo) =>
        seqSimple(SIR.MaybeInit(fieldRefInfo.clazz), Expr.GetStatic(fieldRefInfo) :: stack)
      case OPCode.PutStatic(fieldRefInfo) =>
        stack1(IRType.Any) { (e, r) =>
          val SemanticSave(saveIr, newTempVarCount, saveSubstitutedStack) =
            fieldSave(r, tempVarCount, fieldRefInfo, true)
          Conversion(
            saveIr :+ SIR.MaybeInit(fieldRefInfo.clazz) :+ SIR.SetStatic(fieldRefInfo, e),
            saveSubstitutedStack,
            newTempVarCount
          )
        }
      case OPCode.GetField(fieldRefInfo) =>
        seq(stack1(IRType.AnyRef)((e, r) => (SIR.NotNull(e), Expr.GetField(e, fieldRefInfo) :: r)))
      case OPCode.PutField(fieldRefInfo) =>
        stack2(IRType.AnyRef) { (ep, e, r) =>
          val SemanticSave(saveIr, newTempVarCount, saveSubstitutedStack) =
            fieldSave(r, tempVarCount, fieldRefInfo, false)
          Conversion(
            saveIr :+ SIR.NotNull(e) :+ SIR.SetField(e, ep, fieldRefInfo),
            saveSubstitutedStack,
            newTempVarCount
          )
        }

      case invoke @ OPCode.Invoke(_, _) =>
        handleInvoke(invoke, stack, tempVarCount, pc)

      case OPCode.INVOKEDYNAMIC(indexByte1, indexByte2) => ???

      case OPCode.New(classInfo) =>
        seqSimple(SIR.MaybeInit(classInfo), Expr.UninitializedRef(pc, classInfo) :: stack)

      case OPCode.NewArray(tpe) =>
        stack1(IRType.Int) { (e, r) =>
          Conversion(
            Seq(SIR.NotNegative(e), SIR.NewArray(tempVarCount, e, opToIrType(tpe))),
            Expr.GetFakeLocal(tempVarCount, IRType.Array(opToIrType(tpe))) :: r,
            tempVarCount + 1
          )
        }
      case OPCode.RefNewArray(classInfo) =>
        stack1(IRType.Int) { (e, r) =>
          Conversion(
            Seq(SIR.NotNegative(e), SIR.NewArray(tempVarCount, e, IRType.Ref(classInfo))),
            Expr.GetFakeLocal(tempVarCount, IRType.Array(IRType.Ref(classInfo))) :: r,
            tempVarCount + 1
          )
        }
      case OPCode.MultiRefNewArray(classInfo, dimensions) =>
        if (stack.lengthIs >= dimensions) {
          val (arraySizes, newStack) = stack.splitAt(dimensions)

          val sizesExpr = arraySizes.collect {
            case e: Expr[Int @unchecked] if e.tpe == IRType.Int => e
          }.toVector

          if (arraySizes.lengthIs == sizesExpr.length) {
            val dimExpr  = Expr.ConstTpe(IRType.Int, dimensions)
            val dimTests = Seq(SIR.NotNegative(dimExpr), SIR.NotZero(dimExpr)) ++ sizesExpr.map(s => SIR.NotNegative(s))

            val tpe =
              (0 until dimensions)
                .foldLeft(IRType.Ref(classInfo): IRType)((tpe, _) =>
                  IRType.Array(tpe.asInstanceOf[IRType.Aux[_]]): IRType
                )
                .asInstanceOf[IRType.Aux[Array[_]]]

            Right(
              Conversion(
                dimTests :+ SIR.NewMultiArray(tempVarCount, tpe, sizesExpr),
                Expr.GetFakeLocal(tempVarCount, tpe) :: newStack,
                tempVarCount + 1
              )
            )
          } else Left(s"Found invalid stack type in multi array initialization at $pc")
        } else Left(s"Not enough stack params for multi array initialization at $pc")
      case OPCode.ArrayLength => nop(stack1Add(IRType.AnyArray)(Expr.ArrayLength))

      case OPCode.RefThrow => nopSimple(stack) //TODO
      case OPCode.Cast(classInfo) =>
        stack1(IRType.AnyRef)((e, r) =>
          Conversion(
            Seq(SIR.Cast(tempVarCount, e, IRType.Ref(classInfo))),
            Expr.GetFakeLocal(tempVarCount, IRType.Ref(classInfo)) :: r,
            tempVarCount + 1
          )
        )
      case OPCode.InstanceOf(classInfo) => nop(stack1Add(IRType.AnyRef)(Expr.IsInstanceOf(_, classInfo)))
      case OPCode.MonitorEnter          => seq(stack1(IRType.AnyRef)((e, r) => (SIR.MonitorEnter(e), r)))
      case OPCode.MonitorExit           => seq(stack1(IRType.AnyRef)((e, r) => (SIR.MonitorExit(e), r)))
    }
  }

  def handleInvoke(invoke: OPCode.Invoke, stack: Stack, tempVarCount: Int, pc: Long): Either[String, Conversion] = {
    val nameAndType = invoke.methodRefInfo.nameAndType
    val descriptor  = nameAndType.descriptor
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
          handleInit(params, newStack, tempVarCount, pc)
        else
          handleMethodCall(params, newStack, invoke.methodRefInfo, desc, callType, tempVarCount, pc)
      } else Left(s"Not enough stack params for call at $pc")
    }
  }

  def handleInit(params: Stack, stack: Stack, tempVarCount: Int, pc: Long): Either[String, Conversion] = {
    val paramsInit = params.init
    val paramsLast = params.last
    val paramsVec = paramsInit.toVector.collect {
      case e: Expr[_] if !e.isInstanceOf[Expr.UninitializedRef] => e
    }

    if (paramsInit.lengthIs == paramsVec.length) {
      paramsLast match {
        case uninit @ Expr.UninitializedRef(atAddress, classInfo) =>
          val SemanticSave(saveIr, newTempVarCount, saveSubstitutedStack) = heapSave(stack, tempVarCount)

          val codes = saveIr :+ SIR.New(newTempVarCount, classInfo, paramsVec)
          Right(
            Conversion(
              codes,
              saveSubstitutedStack.map(_.substitute(uninit, Expr.GetFakeLocal(tempVarCount, IRType.Ref(classInfo)))),
              newTempVarCount + 1
            )
          )
        case expr =>
          val SemanticSave(saveIr, newTempVarCount, saveSubstitutedStack) = heapSave(stack, tempVarCount)

          val codes = SIR.NotNull(expr) +: saveIr :+ SIR.CallSuper(expr, paramsVec)
          Right(Conversion(codes, saveSubstitutedStack, newTempVarCount))
      }
    } else Left(s"Found uninitialized reference in object creation call at $pc")
  }

  def handleMethodCall(
      params: Stack,
      stack: Stack,
      methodInfo: MethodInfo,
      desc: Descriptor.MethodDescriptor,
      callType: SIR.CallType,
      tempVarCount: Int,
      pc: Long
  ): Either[String, Conversion] = {
    val paramsVec = params.toVector.reverse.collect {
      case e: Expr[_] if !e.isInstanceOf[Expr.UninitializedRef] => e
    }
    val methodName = methodInfo.nameAndType.name

    if (params.lengthIs == paramsVec.length) {
      val SemanticSave(saveIr, newTempVarCount, saveSubstitutedStack) = heapSave(stack, tempVarCount)
      val codes = if (callType == SIR.CallType.Static) {
        saveIr :+ SIR.Call(newTempVarCount, callType, methodInfo.clazz, methodName, desc, None, paramsVec)
      } else {
        val e = paramsVec.head

        saveIr :+ SIR.NotNull(e) :+ SIR.Call(
          newTempVarCount,
          callType,
          methodInfo.clazz,
          methodName,
          desc,
          Some(e),
          paramsVec.tail
        )
      }

      if (desc.returnType == Descriptor.VoidType) Right(Conversion(codes, saveSubstitutedStack, newTempVarCount + 1))
      else
        Right(
          Conversion(
            codes,
            Expr.GetFakeLocal(newTempVarCount, IRType.Unknown: IRType.Aux[_]) :: saveSubstitutedStack,
            newTempVarCount + 1
          )
        )
    } else Left(s"Found uninitialized reference in method call at $pc")
  }

  sealed trait FieldSaveInfo
  case class InstanceFieldSave(e: Expr[_], field: FieldRefInfo) extends FieldSaveInfo
  case class StaticFieldSave(field: FieldRefInfo)               extends FieldSaveInfo

  def fieldSave(stack: Stack, tempVarCount: Int, fieldInfo: FieldSaveInfo): SemanticSave = {
    val (tpe, toSubstitute) = fieldInfo match {
      case InstanceFieldSave(fieldExpr, field) =>
        (IRType.fromDescriptor(field.nameAndType.descriptor), Expr.GetField(fieldExpr, field))

      case StaticFieldSave(field) =>
        (IRType.fromDescriptor(field.nameAndType.descriptor), Expr.GetStatic(field))
    }

    val substitute: Int => FunctionK[Expr, Expr] = tempVar =>
      new FunctionK[Expr, Expr] {
        override def apply[A](expr: Expr[A]): Expr[A] = expr.substitute(toSubstitute, Expr.GetFakeLocal(tempVar, tpe))
      }

    save(stack, tempVarCount, substitute)
  }

  def heapSave(stack: Stack, tempVarCount: Int): SemanticSave = {
    val substitute: Int => FunctionK[Expr, Expr] = tempVar => {
      lazy val substituteGetField = new FunctionK[Expr, Expr] {
        override def apply[A](expr: Expr[A]): Expr[A] = expr match {
          case Expr.GetField(_, field) =>
            Expr.GetFakeLocal(tempVar, IRType.fromDescriptor(field.nameAndType.descriptor))
          case Expr.GetStatic(field) => Expr.GetFakeLocal(tempVar, IRType.fromDescriptor(field.nameAndType.descriptor))
          case _                     => expr.modifyChildren(substituteGetField)
        }
      }

      substituteGetField
    }

    save(stack, tempVarCount, substitute)
  }

  def save(stack: Stack, tempVarCount: Int, substitute: Int => FunctionK[Expr, Expr]): SemanticSave = {
    val (substitutions, newTempVarCount) = stack.foldLeft((Nil: List[(Expr[_], Expr[_], Boolean, Int)], tempVarCount)) {
      case ((acc, tempVar), expr) =>
        val substitutedExpr = substitute(tempVar)(expr)
        val hadFieldAccess  = substitutedExpr != expr
        val newTempVar      = if (hadFieldAccess) tempVar + 1 else tempVar

        ((expr, substitutedExpr, hadFieldAccess, tempVar) :: acc, newTempVar)
    }

    val saveIr = substitutions.collect {
      case (expr, _, true, tempVarIndex) => SIR.SetFakeLocal(tempVarIndex, expr)
    }.reverse

    val substitutedStack = substitutions.reverseIterator.map {
      case (_, substitution, _, _) => substitution
    }

    SemanticSave(saveIr, newTempVarCount, substitutedStack.to(List))
  }
}
