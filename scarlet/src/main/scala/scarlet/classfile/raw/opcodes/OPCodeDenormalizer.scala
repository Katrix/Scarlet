package scarlet.classfile.raw.opcodes

import cats.data.Validated.Valid
import cats.data.ValidatedNel
import cats.syntax.all._
import scarlet.classfile.denormalized.ConstantPoolEntry.{
  ClassInfo,
  FieldRefInfo,
  InterfaceMethodRefInfo,
  MethodRefInfo
}
import scarlet.classfile.denormalized.opcodes.{OPCode => DeOP}
import scarlet.classfile.denormalized.opcodes.OPCode.{MethodInfo, Type => DeOPType}
import scarlet.classfile.denormalized.{ConstantPool, ConstantPoolEntry}
import scarlet.classfile.raw.opcodes.OPCodeA._
import scodec.Err
import shapeless.Typeable

object OPCodeDenormalizer {

  /**
    * Denormalizes an opcode. This includes resolving references to the
    * constant pool, making address references absolute and collapsing
    * specialization of opcodes.
    */
  def denormalize(code: OPCode, opcodeAddress: Long, pool: ConstantPool): ValidatedNel[Err, DeOP] = {
    def getPool[A <: ConstantPoolEntry: Typeable](indexByte1: Int, indexByte2: Int) =
      pool.get[A]((indexByte1 << 8) | indexByte2)

    def ifInt(ifCond: DeOP.IntIfCond, branchByte1: Int, branchByte2: Int) = {
      val branchShort = (branchByte1 << 8) | branchByte2
      Valid(DeOP.IntIfZero(ifCond, opcodeAddress + branchShort))
    }

    def ifIntCmp(ifCond: DeOP.IntIfCond, branchByte1: Int, branchByte2: Int) = {
      val branchShort = (branchByte1 << 8) | branchByte2
      Valid(DeOP.IntIfCmp(ifCond, opcodeAddress + branchShort))
    }

    def ifRef(ifCond: DeOP.RefIfCond, branchByte1: Int, branchByte2: Int) = {
      val branchShort = (branchByte1 << 8) | branchByte2
      Valid(DeOP.RefIf(ifCond, opcodeAddress + branchShort))
    }

    def ifRefCmp(ifCond: DeOP.RefIfCmpCond, branchByte1: Int, branchByte2: Int) = {
      val branchShort = (branchByte1 << 8) | branchByte2
      Valid(DeOP.RefIfCmp(ifCond, opcodeAddress + branchShort))
    }

    def simplifyMethodRef(info: MethodRefInfo): MethodInfo                   = MethodInfo(info.clazz, info.nameAndType)
    def simplifyInterfaceMethodRef(info: InterfaceMethodRefInfo): MethodInfo = MethodInfo(info.clazz, info.nameAndType)

    code match {
      case NOP                  => Valid(DeOP.Nop)
      case ACONST_NULL          => Valid(DeOP.PushNull)
      case ICONST_M1            => Valid(DeOP.Push(DeOPType.Int, -1))
      case ICONST_0             => Valid(DeOP.Push(DeOPType.Int, 0))
      case ICONST_1             => Valid(DeOP.Push(DeOPType.Int, 1))
      case ICONST_2             => Valid(DeOP.Push(DeOPType.Int, 2))
      case ICONST_3             => Valid(DeOP.Push(DeOPType.Int, 3))
      case ICONST_4             => Valid(DeOP.Push(DeOPType.Int, 4))
      case ICONST_5             => Valid(DeOP.Push(DeOPType.Int, 5))
      case LCONST_0             => Valid(DeOP.Push(DeOPType.Long, 0L))
      case LCONST_1             => Valid(DeOP.Push(DeOPType.Long, 1L))
      case FCONST_0             => Valid(DeOP.Push(DeOPType.Float, 0F))
      case FCONST_1             => Valid(DeOP.Push(DeOPType.Float, 1F))
      case FCONST_2             => Valid(DeOP.Push(DeOPType.Float, 2F))
      case DCONST_0             => Valid(DeOP.Push(DeOPType.Double, 0D))
      case DCONST_1             => Valid(DeOP.Push(DeOPType.Double, 1D))
      case BIPUSH(b)            => Valid(DeOP.Push(DeOPType.Int, b))
      case SIPUSH(byte1, byte2) => Valid(DeOP.Push(DeOPType.Short, ((byte1 << 8) | byte2).toShort))

      case ldc @ (LDC(_) | LDC_W(_, _)) =>
        val idx = ldc match {
          case LDC(index)                    => index
          case LDC_W(indexByte1, indexByte2) => (indexByte1 << 8) | indexByte2
          case _                             => sys.error("impossible")
        }

        pool.getAny(idx).andThen {
          case ConstantPoolEntry.IntegerInfo(v) => Valid(DeOP.Push(DeOPType.Int, v))
          case ConstantPoolEntry.FloatInfo(v)   => Valid(DeOP.Push(DeOPType.Float, v))
          case ConstantPoolEntry.StringInfo(v)  => Valid(DeOP.Push(DeOPType.String, v))
          case ConstantPoolEntry.ClassInfo(tpe) =>
            val refTpe = if (!tpe.startsWith("[")) s"L$tpe;" else tpe

            Valid(DeOP.Push(DeOPType.Class, refTpe))
          case entry =>
            Err(
              s"Tried to get int, float, string or ??? at index $idx in constant " +
                s"pool, but found ${entry.getClass.getSimpleName}"
            ).invalidNel
        }

      case LDC2_W(indexByte1, indexByte2) =>
        val idx = (indexByte1 << 8) | indexByte2
        pool
          .getAny(idx)
          .andThen {
            case ConstantPoolEntry.LongInfo(v)   => Valid(DeOP.Push(DeOPType.Long, v))
            case ConstantPoolEntry.DoubleInfo(v) => Valid(DeOP.Push(DeOPType.Double, v))
            case entry =>
              Err(
                s"Tried to get long or double at index $idx in constant " +
                  s"pool, but found ${entry.getClass.getSimpleName}"
              ).invalidNel
          }

      case ILOAD(index) => Valid(DeOP.VarLoad(DeOPType.Int, index))
      case LLOAD(index) => Valid(DeOP.VarLoad(DeOPType.Long, index))
      case FLOAD(index) => Valid(DeOP.VarLoad(DeOPType.Float, index))
      case DLOAD(index) => Valid(DeOP.VarLoad(DeOPType.Double, index))
      case ALOAD(index) => Valid(DeOP.VarLoad(DeOPType.Ref, index))
      case ILOAD_0      => Valid(DeOP.VarLoad(DeOPType.Int, 0))
      case ILOAD_1      => Valid(DeOP.VarLoad(DeOPType.Int, 1))
      case ILOAD_2      => Valid(DeOP.VarLoad(DeOPType.Int, 2))
      case ILOAD_3      => Valid(DeOP.VarLoad(DeOPType.Int, 3))
      case LLOAD_0      => Valid(DeOP.VarLoad(DeOPType.Long, 0))
      case LLOAD_1      => Valid(DeOP.VarLoad(DeOPType.Long, 1))
      case LLOAD_2      => Valid(DeOP.VarLoad(DeOPType.Long, 2))
      case LLOAD_3      => Valid(DeOP.VarLoad(DeOPType.Long, 3))
      case FLOAD_0      => Valid(DeOP.VarLoad(DeOPType.Float, 0))
      case FLOAD_1      => Valid(DeOP.VarLoad(DeOPType.Float, 1))
      case FLOAD_2      => Valid(DeOP.VarLoad(DeOPType.Float, 2))
      case FLOAD_3      => Valid(DeOP.VarLoad(DeOPType.Float, 3))
      case DLOAD_0      => Valid(DeOP.VarLoad(DeOPType.Double, 0))
      case DLOAD_1      => Valid(DeOP.VarLoad(DeOPType.Double, 1))
      case DLOAD_2      => Valid(DeOP.VarLoad(DeOPType.Double, 2))
      case DLOAD_3      => Valid(DeOP.VarLoad(DeOPType.Double, 3))
      case ALOAD_0      => Valid(DeOP.VarLoad(DeOPType.Ref, 0))
      case ALOAD_1      => Valid(DeOP.VarLoad(DeOPType.Ref, 1))
      case ALOAD_2      => Valid(DeOP.VarLoad(DeOPType.Ref, 2))
      case ALOAD_3      => Valid(DeOP.VarLoad(DeOPType.Ref, 3))

      case IALOAD => Valid(DeOP.ArrayLoad(DeOPType.Int))
      case LALOAD => Valid(DeOP.ArrayLoad(DeOPType.Long))
      case FALOAD => Valid(DeOP.ArrayLoad(DeOPType.Float))
      case DALOAD => Valid(DeOP.ArrayLoad(DeOPType.Double))
      case AALOAD => Valid(DeOP.ArrayLoad(DeOPType.Ref))
      case BALOAD => Valid(DeOP.ArrayLoad(DeOPType.Byte))
      case CALOAD => Valid(DeOP.ArrayLoad(DeOPType.Char))
      case SALOAD => Valid(DeOP.ArrayLoad(DeOPType.Short))

      case ISTORE(index) => Valid(DeOP.VarStore(DeOPType.Int, index))
      case LSTORE(index) => Valid(DeOP.VarStore(DeOPType.Long, index))
      case FSTORE(index) => Valid(DeOP.VarStore(DeOPType.Float, index))
      case DSTORE(index) => Valid(DeOP.VarStore(DeOPType.Double, index))
      case ASTORE(index) => Valid(DeOP.VarStore(DeOPType.Ref, index))
      case ISTORE_0      => Valid(DeOP.VarStore(DeOPType.Int, 0))
      case ISTORE_1      => Valid(DeOP.VarStore(DeOPType.Int, 1))
      case ISTORE_2      => Valid(DeOP.VarStore(DeOPType.Int, 2))
      case ISTORE_3      => Valid(DeOP.VarStore(DeOPType.Int, 3))
      case LSTORE_0      => Valid(DeOP.VarStore(DeOPType.Long, 0))
      case LSTORE_1      => Valid(DeOP.VarStore(DeOPType.Long, 1))
      case LSTORE_2      => Valid(DeOP.VarStore(DeOPType.Long, 2))
      case LSTORE_3      => Valid(DeOP.VarStore(DeOPType.Long, 3))
      case FSTORE_0      => Valid(DeOP.VarStore(DeOPType.Float, 0))
      case FSTORE_1      => Valid(DeOP.VarStore(DeOPType.Float, 1))
      case FSTORE_2      => Valid(DeOP.VarStore(DeOPType.Float, 2))
      case FSTORE_3      => Valid(DeOP.VarStore(DeOPType.Float, 3))
      case DSTORE_0      => Valid(DeOP.VarStore(DeOPType.Double, 0))
      case DSTORE_1      => Valid(DeOP.VarStore(DeOPType.Double, 1))
      case DSTORE_2      => Valid(DeOP.VarStore(DeOPType.Double, 2))
      case DSTORE_3      => Valid(DeOP.VarStore(DeOPType.Double, 3))
      case ASTORE_1      => Valid(DeOP.VarStore(DeOPType.Ref, 0))
      case ASTORE_2      => Valid(DeOP.VarStore(DeOPType.Ref, 1))
      case ASTORE_3      => Valid(DeOP.VarStore(DeOPType.Ref, 2))
      case ASTORE_4      => Valid(DeOP.VarStore(DeOPType.Ref, 3))

      case IASTORE => Valid(DeOP.ArrayStore(DeOPType.Int))
      case LASTORE => Valid(DeOP.ArrayStore(DeOPType.Long))
      case FASTORE => Valid(DeOP.ArrayStore(DeOPType.Float))
      case DASTORE => Valid(DeOP.ArrayStore(DeOPType.Double))
      case AASTORE => Valid(DeOP.ArrayStore(DeOPType.Ref))
      case BASTORE => Valid(DeOP.ArrayStore(DeOPType.Byte))
      case CASTORE => Valid(DeOP.ArrayStore(DeOPType.Char))
      case SASTORE => Valid(DeOP.ArrayStore(DeOPType.Short))

      case POP     => Valid(DeOP.Pop1)
      case POP2    => Valid(DeOP.Pop2)
      case DUP     => Valid(DeOP.Dup)
      case DUP_X1  => Valid(DeOP.DupX1)
      case DUP_X2  => Valid(DeOP.DupX2)
      case DUP2    => Valid(DeOP.Dup2)
      case DUP2_X1 => Valid(DeOP.Dup2X1)
      case DUP2_X2 => Valid(DeOP.Dup2X2)
      case SWAP    => Valid(DeOP.Swap)

      case IADD  => Valid(DeOP.Add(DeOPType.Int))
      case LADD  => Valid(DeOP.Add(DeOPType.Long))
      case FADD  => Valid(DeOP.Add(DeOPType.Float))
      case DADD  => Valid(DeOP.Add(DeOPType.Double))
      case ISUB  => Valid(DeOP.Sub(DeOPType.Int))
      case LSUB  => Valid(DeOP.Sub(DeOPType.Long))
      case FSUB  => Valid(DeOP.Sub(DeOPType.Float))
      case DSUB  => Valid(DeOP.Sub(DeOPType.Double))
      case IMUL  => Valid(DeOP.Mult(DeOPType.Int))
      case LMUL  => Valid(DeOP.Mult(DeOPType.Long))
      case FMUL  => Valid(DeOP.Mult(DeOPType.Float))
      case DMUL  => Valid(DeOP.Mult(DeOPType.Double))
      case IDIV  => Valid(DeOP.Div(DeOPType.Int))
      case LDIV  => Valid(DeOP.Div(DeOPType.Long))
      case FDIV  => Valid(DeOP.Div(DeOPType.Float))
      case DDIV  => Valid(DeOP.Div(DeOPType.Double))
      case IREM  => Valid(DeOP.Rem(DeOPType.Int))
      case LREM  => Valid(DeOP.Rem(DeOPType.Long))
      case FREM  => Valid(DeOP.Rem(DeOPType.Float))
      case DREM  => Valid(DeOP.Rem(DeOPType.Double))
      case INEG  => Valid(DeOP.Neg(DeOPType.Int))
      case LNEG  => Valid(DeOP.Neg(DeOPType.Long))
      case FNEG  => Valid(DeOP.Neg(DeOPType.Float))
      case DNEG  => Valid(DeOP.Neg(DeOPType.Double))
      case ISHL  => Valid(DeOP.ShiftLeft(DeOPType.Int))
      case LSHL  => Valid(DeOP.ShiftLeft(DeOPType.Long))
      case ISHR  => Valid(DeOP.ShiftRight(DeOPType.Int))
      case LSHR  => Valid(DeOP.ShiftRight(DeOPType.Long))
      case LUSHR => Valid(DeOP.LogShiftRight(DeOPType.Long))
      case IUSHR => Valid(DeOP.LogShiftRight(DeOPType.Int))
      case IAND  => Valid(DeOP.And(DeOPType.Int))
      case LAND  => Valid(DeOP.And(DeOPType.Long))
      case IOR   => Valid(DeOP.Or(DeOPType.Int))
      case LOR   => Valid(DeOP.Or(DeOPType.Long))
      case IXOR  => Valid(DeOP.Xor(DeOPType.Int))
      case LXOR  => Valid(DeOP.Xor(DeOPType.Long))

      case IINC(index, const) => Valid(DeOP.IntVarIncr(index, const))

      case I2L => Valid(DeOP.Conversion(DeOPType.Int, DeOPType.Long))
      case I2F => Valid(DeOP.Conversion(DeOPType.Int, DeOPType.Float))
      case I2D => Valid(DeOP.Conversion(DeOPType.Int, DeOPType.Double))
      case L2I => Valid(DeOP.Conversion(DeOPType.Long, DeOPType.Int))
      case L2F => Valid(DeOP.Conversion(DeOPType.Long, DeOPType.Float))
      case L2D => Valid(DeOP.Conversion(DeOPType.Long, DeOPType.Double))
      case F2I => Valid(DeOP.Conversion(DeOPType.Float, DeOPType.Int))
      case F2L => Valid(DeOP.Conversion(DeOPType.Float, DeOPType.Long))
      case F2D => Valid(DeOP.Conversion(DeOPType.Float, DeOPType.Double))
      case D2I => Valid(DeOP.Conversion(DeOPType.Double, DeOPType.Int))
      case D2L => Valid(DeOP.Conversion(DeOPType.Double, DeOPType.Long))
      case D2F => Valid(DeOP.Conversion(DeOPType.Double, DeOPType.Float))
      case I2B => Valid(DeOP.Conversion(DeOPType.Int, DeOPType.Byte))
      case I2C => Valid(DeOP.Conversion(DeOPType.Int, DeOPType.Char))
      case I2S => Valid(DeOP.Conversion(DeOPType.Int, DeOPType.Short))

      case LCMP  => Valid(DeOP.Compare(DeOPType.Long, DeOP.NanBehavior.IsIntegral))
      case FCMPL => Valid(DeOP.Compare(DeOPType.Float, DeOP.NanBehavior.NanSmall))
      case FCMPG => Valid(DeOP.Compare(DeOPType.Float, DeOP.NanBehavior.NanBig))
      case DCMPL => Valid(DeOP.Compare(DeOPType.Double, DeOP.NanBehavior.NanSmall))
      case DCMPG => Valid(DeOP.Compare(DeOPType.Double, DeOP.NanBehavior.NanBig))

      case IFEQ(branchByte1, branchByte2)      => ifInt(DeOP.IntIfCond.EQ, branchByte1, branchByte2)
      case IFNE(branchByte1, branchByte2)      => ifInt(DeOP.IntIfCond.NE, branchByte1, branchByte2)
      case IFLT(branchByte1, branchByte2)      => ifInt(DeOP.IntIfCond.LT, branchByte1, branchByte2)
      case IFGE(branchByte1, branchByte2)      => ifInt(DeOP.IntIfCond.GE, branchByte1, branchByte2)
      case IFGT(branchByte1, branchByte2)      => ifInt(DeOP.IntIfCond.GT, branchByte1, branchByte2)
      case IFLE(branchByte1, branchByte2)      => ifInt(DeOP.IntIfCond.LE, branchByte1, branchByte2)
      case IF_ICMPEQ(branchByte1, branchByte2) => ifIntCmp(DeOP.IntIfCond.EQ, branchByte1, branchByte2)
      case IF_ICMPNE(branchByte1, branchByte2) => ifIntCmp(DeOP.IntIfCond.NE, branchByte1, branchByte2)
      case IF_ICMPLT(branchByte1, branchByte2) => ifIntCmp(DeOP.IntIfCond.LT, branchByte1, branchByte2)
      case IF_ICMPGE(branchByte1, branchByte2) => ifIntCmp(DeOP.IntIfCond.GE, branchByte1, branchByte2)
      case IF_ICMPGT(branchByte1, branchByte2) => ifIntCmp(DeOP.IntIfCond.GT, branchByte1, branchByte2)
      case IF_ICMPLE(branchByte1, branchByte2) => ifIntCmp(DeOP.IntIfCond.LE, branchByte1, branchByte2)
      case IF_ACMPEQ(branchByte1, branchByte2) => ifRefCmp(DeOP.RefIfCmpCond.EQ, branchByte1, branchByte2)
      case IF_ACMPNE(branchByte1, branchByte2) => ifRefCmp(DeOP.RefIfCmpCond.NE, branchByte1, branchByte2)

      case GOTO(branchByte1, branchByte2) => Valid(DeOP.Goto(((branchByte1 << 8) | branchByte2) + opcodeAddress))
      case JSR(_, _)                      => Err("Unsupported OPCODE JSR").invalidNel
      case RET(_)                         => Err("Unsupported OPCODE RET").invalidNel

      case LOOKUPSWITCH(
          defaultbyte1,
          defaultbyte2,
          defaultbyte3,
          defaultbyte4,
          _,
          _,
          _,
          _,
          pairs
          ) =>
        val default = ((defaultbyte1 << 24) | (defaultbyte2 << 16) | (defaultbyte3 << 8) | defaultbyte4) + opcodeAddress

        val intPairs = pairs.grouped(8).map { v =>
          val p1 = (v(0) << 24) | (v(1) << 16) | (v(2) << 8) | v(3)
          val p2 = (v(4) << 24) | (v(5) << 16) | (v(6) << 8) | v(7)
          (p1, p2 + opcodeAddress)
        }

        Valid(DeOP.Switch(default, intPairs.toVector))
      case TABLESWITCH(
          defaultbyte1,
          defaultbyte2,
          defaultbyte3,
          defaultbyte4,
          lowbyte1,
          lowbyte2,
          lowbyte3,
          lowbyte4,
          _,
          _,
          _,
          _,
          offsets
          ) =>
        val default = ((defaultbyte1 << 24) | (defaultbyte2 << 16) | (defaultbyte3 << 8) | defaultbyte4) + opcodeAddress
        val low     = (lowbyte1 << 24) | (lowbyte2 << 16) | (lowbyte3 << 8) | lowbyte4

        val intPairs = offsets.grouped(4).zipWithIndex.map {
          case (v, i) =>
            val p1 = low + i
            val p2 = (v(0) << 24) | (v(1) << 16) | (v(2) << 8) | v(3)
            (p1, p2 + opcodeAddress)
        }

        Valid(DeOP.Switch(default, intPairs.toVector))
      case IRETURN => Valid(DeOP.Return(Some(DeOPType.Int)))
      case LRETURN => Valid(DeOP.Return(Some(DeOPType.Long)))
      case FRETURN => Valid(DeOP.Return(Some(DeOPType.Float)))
      case DRETURN => Valid(DeOP.Return(Some(DeOPType.Double)))
      case ARETURN => Valid(DeOP.Return(Some(DeOPType.Ref)))
      case RETURN  => Valid(DeOP.Return(None))

      case GETSTATIC(idx1, idx2) => getPool[FieldRefInfo](idx1, idx2).map(DeOP.GetStatic)
      case PUTSTATIC(idx1, idx2) => getPool[FieldRefInfo](idx1, idx2).map(DeOP.PutStatic)
      case GETFIELD(idx1, idx2)  => getPool[FieldRefInfo](idx1, idx2).map(DeOP.GetField)
      case PUTFIELD(idx1, idx2)  => getPool[FieldRefInfo](idx1, idx2).map(DeOP.PutField)

      case INVOKEVIRTUAL(idx1, idx2) =>
        getPool[MethodRefInfo](idx1, idx2).map(simplifyMethodRef).map(DeOP.Invoke(_, DeOP.InvokeType.Virtual))
      case INVOKESPECIAL(idx1, idx2) =>
        val index = (idx1 << 8) | idx2
        pool.getAny(index).andThen {
          case ref: MethodRefInfo => Valid(DeOP.Invoke(simplifyMethodRef(ref), DeOP.InvokeType.Special))
          case ref: InterfaceMethodRefInfo =>
            Valid(DeOP.Invoke(simplifyInterfaceMethodRef(ref), DeOP.InvokeType.Special))
          case entry =>
            Err(
              s"Tried to get method or interface method at index $index in constant " +
                s"pool, but found ${entry.getClass.getSimpleName}"
            ).invalidNel
        }

      case INVOKESTATIC(idx1, idx2) =>
        val index = (idx1 << 8) | idx2
        pool.getAny(index).andThen {
          case ref: MethodRefInfo => Valid(DeOP.Invoke(simplifyMethodRef(ref), DeOP.InvokeType.Static))
          case ref: InterfaceMethodRefInfo =>
            Valid(DeOP.Invoke(simplifyInterfaceMethodRef(ref), DeOP.InvokeType.Static))
          case entry =>
            Err(
              s"Tried to get method or interface method at index $index in constant " +
                s"pool, but found ${entry.getClass.getSimpleName}"
            ).invalidNel
        }

      case INVOKEINTERFACE(idx1, idx2, _, _) =>
        getPool[InterfaceMethodRefInfo](idx1, idx2)
          .map(simplifyInterfaceMethodRef)
          .map(DeOP.Invoke(_, DeOP.InvokeType.Interface))
      case INVOKEDYNAMIC(idx1, idx2, _, _) => ???

      case NEW(idx1, idx2) => getPool[ClassInfo](idx1, idx2).map(DeOP.New)
      case NEWARRAY(atype) =>
        val tpe = atype match {
          case 4  => Valid(DeOPType.Boolean)
          case 5  => Valid(DeOPType.Char)
          case 6  => Valid(DeOPType.Float)
          case 7  => Valid(DeOPType.Double)
          case 8  => Valid(DeOPType.Byte)
          case 9  => Valid(DeOPType.Short)
          case 10 => Valid(DeOPType.Int)
          case 11 => Valid(DeOPType.Long)
          case _  => Err("Unknown array type").invalidNel
        }

        tpe.map(DeOP.NewArray)

      case ANEWARRAY(idx1, idx2) => getPool[ClassInfo](idx1, idx2).map(DeOP.RefNewArray)
      case ARRAYLENGTH           => Valid(DeOP.ArrayLength)

      case ATHROW                 => Valid(DeOP.RefThrow)
      case CHECKCAST(idx1, idx2)  => getPool[ClassInfo](idx1, idx2).map(DeOP.Cast)
      case INSTANCEOF(idx1, idx2) => getPool[ClassInfo](idx1, idx2).map(DeOP.InstanceOf)

      case MONITORENTER => Valid(DeOP.MonitorEnter)
      case MONITOREXIT  => Valid(DeOP.MonitorExit)

      case wide: WIDE[Int] =>
        wide match {
          case WIDELOADSTORERET(id, idx1, idx2) if id == ILOAD(()).id =>
            Valid(DeOP.VarLoad(DeOPType.Int, (idx1 << 8) | idx2))
          case WIDELOADSTORERET(id, idx1, idx2) if id == FLOAD(()).id =>
            Valid(DeOP.VarLoad(DeOPType.Float, (idx1 << 8) | idx2))
          case WIDELOADSTORERET(id, idx1, idx2) if id == ALOAD(()).id =>
            Valid(DeOP.VarLoad(DeOPType.Ref, (idx1 << 8) | idx2))
          case WIDELOADSTORERET(id, idx1, idx2) if id == LLOAD(()).id =>
            Valid(DeOP.VarLoad(DeOPType.Long, (idx1 << 8) | idx2))
          case WIDELOADSTORERET(id, idx1, idx2) if id == DLOAD(()).id =>
            Valid(DeOP.VarLoad(DeOPType.Double, (idx1 << 8) | idx2))
          case WIDELOADSTORERET(id, idx1, idx2) if id == ISTORE(()).id =>
            Valid(DeOP.VarStore(DeOPType.Int, (idx1 << 8) | idx2))
          case WIDELOADSTORERET(id, idx1, idx2) if id == FSTORE(()).id =>
            Valid(DeOP.VarStore(DeOPType.Float, (idx1 << 8) | idx2))
          case WIDELOADSTORERET(id, idx1, idx2) if id == ASTORE(()).id =>
            Valid(DeOP.VarStore(DeOPType.Ref, (idx1 << 8) | idx2))
          case WIDELOADSTORERET(id, idx1, idx2) if id == LSTORE(()).id =>
            Valid(DeOP.VarStore(DeOPType.Long, (idx1 << 8) | idx2))
          case WIDELOADSTORERET(id, idx1, idx2) if id == DSTORE(()).id =>
            Valid(DeOP.VarStore(DeOPType.Double, (idx1 << 8) | idx2))
          case WIDELOADSTORERET(id, _, _) if id == RET(()).id => Err("Unsupported OPCODE wide RET").invalidNel
          case WIDELOADSTORERET(id, _, _)                     => Err(s"Unknown wide instruction $id").invalidNel
          case WIDEIINC(idx1, idx2, constByte1, constByte2) =>
            Valid(DeOP.IntVarIncr((idx1 << 8) | idx2, (constByte1 << 8) | constByte2))
        }

      case MULTIANEWARRAY(idx1, idx2, dimensions) =>
        getPool[ClassInfo](idx1, idx2).map(DeOP.MultiRefNewArray(_, dimensions))

      case IFNULL(branchByte1, branchByte2)    => ifRef(DeOP.RefIfCond.IsNull, branchByte1, branchByte2)
      case IFNONNULL(branchByte1, branchByte2) => ifRef(DeOP.RefIfCond.IsNotNull, branchByte1, branchByte2)

      case GOTO_W(branchByte1, branchByte2, branchByte3, branchByte4) =>
        Valid(DeOP.Goto(((branchByte1 << 24) | (branchByte2 << 16) | (branchByte3 << 8) | branchByte4) + opcodeAddress))
      case JSR_W(_, _, _, _) =>
        Err("Unsupported OPCODE JSR_W").invalidNel

      case BREAKPOINT => Err("Found invalid OPCode BREAKPOINT in class").invalidNel
      case IMPDEP1    => Err("Found invalid OPCode IMPDEP1 in class").invalidNel
      case IMPDEP2    => Err("Found invalid OPCode IMPDEP2 in class").invalidNel
    }
  }
}
