package scarlet.classfile.denormalized.opcodes

import scarlet.classfile.denormalized.ConstantPoolEntry.{ClassInfo, FieldRefInfo, NameAndTypeInfo}

/**
  * Represents a denormalized opcode. Abstracts over types, and does not contain
  * any specialization compared to it's raw counterpart.
  */
sealed trait OPCode
object OPCode {

  /**
    * The type an opcode operates on.
    */
  trait Type {
    type A
  }
  object Type {
    type Aux[A0] = Type { type A = A0 }

    case object Boolean extends Type { type A = scala.Boolean }
    case object Byte    extends Type { type A = scala.Byte    }
    case object Short   extends Type { type A = scala.Short   }
    case object Int     extends Type { type A = scala.Int     }
    case object Long    extends Type { type A = scala.Long    }
    case object Float   extends Type { type A = scala.Float   }
    case object Double  extends Type { type A = scala.Double  }
    case object Char    extends Type { type A = scala.Char    }
    case object String  extends Type { type A = Predef.String }
    case object Class   extends Type { type A = Predef.String }
    case object Ref     extends Type { type A = scala.AnyRef  }
  }

  case object Nop      extends OPCode
  case object PushNull extends OPCode

  case class Push[A](tpe: Type.Aux[A], value: A) extends OPCode

  case class VarLoad(tpe: Type, index: Int)  extends OPCode
  case class ArrayLoad(tpe: Type)            extends OPCode
  case class VarStore(tpe: Type, index: Int) extends OPCode
  case class ArrayStore(tpe: Type)           extends OPCode

  case object Pop1   extends OPCode
  case object Pop2   extends OPCode
  case object Dup    extends OPCode
  case object DupX1  extends OPCode
  case object DupX2  extends OPCode
  case object Dup2   extends OPCode
  case object Dup2X1 extends OPCode
  case object Dup2X2 extends OPCode
  case object Swap   extends OPCode

  case class Add(tpe: Type)  extends OPCode
  case class Sub(tpe: Type)  extends OPCode
  case class Mult(tpe: Type) extends OPCode
  case class Div(tpe: Type)  extends OPCode
  case class Rem(tpe: Type)  extends OPCode
  case class Neg(tpe: Type)  extends OPCode

  case class ShiftLeft(tpe: Type)     extends OPCode
  case class ShiftRight(tpe: Type)    extends OPCode
  case class LogShiftRight(tpe: Type) extends OPCode

  case class And(tpe: Type) extends OPCode
  case class Or(tpe: Type)  extends OPCode
  case class Xor(tpe: Type) extends OPCode

  case class IntVarIncr(index: Int, amount: Int) extends OPCode

  case class Conversion(from: Type, to: Type) extends OPCode

  case class Compare(tpe: Type, nanBehavior: NanBehavior) extends OPCode
  sealed trait NanBehavior
  object NanBehavior {
    case object IsIntegral extends NanBehavior
    case object NanSmall   extends NanBehavior
    case object NanBig     extends NanBehavior
  }

  case class IntIfZero(cond: IntIfCond, branchPC: Long)   extends OPCode
  case class IntIfCmp(cond: IntIfCond, branchPC: Long)    extends OPCode
  case class RefIf(cond: RefIfCond, branchPC: Long)       extends OPCode
  case class RefIfCmp(cond: RefIfCmpCond, branchPC: Long) extends OPCode

  sealed trait IntIfCond
  object IntIfCond {
    case object EQ extends IntIfCond
    case object NE extends IntIfCond
    case object LT extends IntIfCond
    case object GE extends IntIfCond
    case object GT extends IntIfCond
    case object LE extends IntIfCond
  }

  sealed trait RefIfCond
  object RefIfCond {
    case object IsNull    extends RefIfCond
    case object IsNotNull extends RefIfCond
  }

  sealed trait RefIfCmpCond
  object RefIfCmpCond {
    case object EQ extends RefIfCmpCond
    case object NE extends RefIfCmpCond
  }

  case class Goto(branchPC: Long)                                extends OPCode
  case class Switch(defaultPC: Long, pairs: Vector[(Int, Long)]) extends OPCode
  case class Return(tpe: Option[Type])                           extends OPCode

  case class GetStatic(fieldRefInfo: FieldRefInfo) extends OPCode
  case class PutStatic(fieldRefInfo: FieldRefInfo) extends OPCode
  case class GetField(fieldRefInfo: FieldRefInfo)  extends OPCode
  case class PutField(fieldRefInfo: FieldRefInfo)  extends OPCode

  sealed trait InvokeType
  object InvokeType {
    case object Virtual   extends InvokeType
    case object Special   extends InvokeType
    case object Static    extends InvokeType
    case object Interface extends InvokeType
  }

  case class Invoke(methodRefInfo: MethodInfo, invokeType: InvokeType) extends OPCode
  case class INVOKEDYNAMIC[A](indexByte1: A, indexByte2: A)            extends OPCode //TODO

  case class MethodInfo(clazz: ClassInfo, nameAndType: NameAndTypeInfo)

  case class New(classInfo: ClassInfo)                               extends OPCode
  case class NewArray(tpe: Type)                                     extends OPCode
  case class RefNewArray(classInfo: ClassInfo)                       extends OPCode
  case class MultiRefNewArray(classInfo: ClassInfo, dimensions: Int) extends OPCode
  case object ArrayLength                                            extends OPCode

  case object RefThrow                        extends OPCode
  case class Cast(classInfo: ClassInfo)       extends OPCode
  case class InstanceOf(classInfo: ClassInfo) extends OPCode
  case object MonitorEnter                    extends OPCode
  case object MonitorExit                     extends OPCode
}
