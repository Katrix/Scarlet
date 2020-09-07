package scarlet.classfile.denormalized

import scarlet.classfile.shared.ConstantPoolTag

sealed abstract class ConstantPoolEntry(val tag: ConstantPoolTag)
object ConstantPoolEntry {
  case class ClassInfo(name: String) extends ConstantPoolEntry(ConstantPoolTag.Class)
  case class FieldRefInfo(clazz: ClassInfo, nameAndType: NameAndTypeInfo)
      extends ConstantPoolEntry(ConstantPoolTag.FieldRef)
  case class MethodRefInfo(clazz: ClassInfo, nameAndType: NameAndTypeInfo)
      extends ConstantPoolEntry(ConstantPoolTag.MethodRef)
  case class InterfaceMethodRefInfo(clazz: ClassInfo, nameAndType: NameAndTypeInfo)
      extends ConstantPoolEntry(ConstantPoolTag.InterfaceMethodRef)

  sealed trait PoolValueConstant[A] extends ConstantPoolEntry {
    def value: A
  }

  case class StringInfo(value: String) extends ConstantPoolEntry(ConstantPoolTag.String) with PoolValueConstant[String]
  case class IntegerInfo(value: Int)   extends ConstantPoolEntry(ConstantPoolTag.Integer) with PoolValueConstant[Int]
  case class FloatInfo(value: Float)   extends ConstantPoolEntry(ConstantPoolTag.Float) with PoolValueConstant[Float]
  case class LongInfo(value: Long)     extends ConstantPoolEntry(ConstantPoolTag.Long) with PoolValueConstant[Long]
  case class DoubleInfo(value: Double) extends ConstantPoolEntry(ConstantPoolTag.Double) with PoolValueConstant[Double]
  case class NameAndTypeInfo(name: String, descriptor: Descriptor)
      extends ConstantPoolEntry(ConstantPoolTag.NameAndType)
  case class Utf8Info(string: String) extends ConstantPoolEntry(ConstantPoolTag.Utf8)

  sealed abstract class MethodHandleInfo                                                    extends ConstantPoolEntry(ConstantPoolTag.MethodHandle)
  case class FieldMethodHandleInfo(refKind: RefKind.FieldRefKind, refInfo: FieldRefInfo)    extends MethodHandleInfo
  case class MethodMethodHandleInfo(refKind: RefKind.MethodRefKind, refInfo: MethodRefInfo) extends MethodHandleInfo
  case class InterfaceMethodOrMethodMethodHandleInfo(
      refKind: RefKind.InterfaceMethodOrMethodRefKind,
      refInfo: Either[MethodRefInfo, InterfaceMethodRefInfo]
  ) extends MethodHandleInfo
  case class InterfaceMethodMethodHandleInfo(refKind: RefKind.InterfaceMethodRefKind, refInfo: InterfaceMethodRefInfo)
      extends MethodHandleInfo

  sealed abstract class RefKind(val value: Int)
  object RefKind {
    sealed abstract class FieldRefKind(value: Int)                   extends RefKind(value)
    sealed abstract class MethodRefKind(value: Int)                  extends RefKind(value)
    sealed abstract class InterfaceMethodOrMethodRefKind(value: Int) extends RefKind(value)
    sealed abstract class InterfaceMethodRefKind(value: Int)         extends RefKind(value)

    case object GetField  extends FieldRefKind(1)
    case object GetStatic extends FieldRefKind(2)
    case object PutField  extends FieldRefKind(3)
    case object PutStatic extends FieldRefKind(4)

    case object InvokeVirtual    extends MethodRefKind(5)
    case object NewInvokeSpecial extends MethodRefKind(8)

    case object InvokeStatic  extends InterfaceMethodOrMethodRefKind(6)
    case object InvokeSpecial extends InterfaceMethodOrMethodRefKind(7)

    case object InvokeInterface extends InterfaceMethodRefKind(9)

    def fromInt(int: Int): Option[RefKind] = int match {
      case 1 => Some(GetField)
      case 2 => Some(GetStatic)
      case 3 => Some(PutField)
      case 4 => Some(PutStatic)
      case 5 => Some(InvokeVirtual)
      case 6 => Some(InvokeStatic)
      case 7 => Some(InvokeSpecial)
      case 8 => Some(NewInvokeSpecial)
      case 9 => Some(InvokeInterface)
      case _ => None
    }
  }

  case class MethodTypeInfo(descriptor: Descriptor.MethodDescriptor)
      extends ConstantPoolEntry(ConstantPoolTag.MethodType)
  case class InvokeDynamicInfo(bootstrapMethodAttrIdx: Int, nameAndType: NameAndTypeInfo)
      extends ConstantPoolEntry(ConstantPoolTag.InvokeDynamic)
}
