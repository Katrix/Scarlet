package scarlet.classfile.raw

import scarlet.classfile.shared.ConstantPoolTag
import scodec.bits.ByteVector

sealed abstract class ConstantPoolEntry(val tag: ConstantPoolTag)
object ConstantPoolEntry {
  case class ClassInfo(nameIdx: Int)                           extends ConstantPoolEntry(ConstantPoolTag.Class)
  case class FieldRefInfo(classIdx: Int, nameAndTypeIdx: Int)  extends ConstantPoolEntry(ConstantPoolTag.FieldRef)
  case class MethodRefInfo(classIdx: Int, nameAndTypeIdx: Int) extends ConstantPoolEntry(ConstantPoolTag.MethodRef)
  case class InterfaceMethodRefInfo(classIdx: Int, nameAndTypeIdx: Int)
      extends ConstantPoolEntry(ConstantPoolTag.InterfaceMethodRef)
  case class StringInfo(stringIdx: Int)                        extends ConstantPoolEntry(ConstantPoolTag.String)
  case class IntegerInfo(bytes: Long)                          extends ConstantPoolEntry(ConstantPoolTag.Integer)
  case class FloatInfo(bytes: Long)                            extends ConstantPoolEntry(ConstantPoolTag.Float)
  case class LongInfo(highBytes: Long, lowBytes: Long)         extends ConstantPoolEntry(ConstantPoolTag.Long)
  case class DoubleInfo(highBytes: Long, lowBytes: Long)       extends ConstantPoolEntry(ConstantPoolTag.Double)
  case class NameAndTypeInfo(nameIdx: Int, descriptorIdx: Int) extends ConstantPoolEntry(ConstantPoolTag.NameAndType)
  case class Utf8Info(bytes: ByteVector)                       extends ConstantPoolEntry(ConstantPoolTag.Utf8)
  case class MethodHandleInfo(referenceKind: Int, referenceIdx: Int)
      extends ConstantPoolEntry(ConstantPoolTag.MethodHandle)
  case class MethodTypeInfo(descriptorIdx: Int) extends ConstantPoolEntry(ConstantPoolTag.MethodType)
  case class InvokeDynamicInfo(bootstrapMethodAttrIdx: Int, nameAndTypeIdx: Int)
      extends ConstantPoolEntry(ConstantPoolTag.InvokeDynamic)
}
