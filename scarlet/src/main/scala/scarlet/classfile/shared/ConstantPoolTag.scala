package scarlet.classfile.shared

import scarlet.classfile.raw.ConstantPoolEntry

import scodec._
import scodec.bits._
import scodec.codecs._

/**
  * The type of the content of the constant pool.
  * @param value The id of this type
  * @param size The size of this entry type
  */
sealed abstract class ConstantPoolTag(val value: Int, val size: Int, private val rawCodec: Codec[ConstantPoolEntry]) {

  /**
    * A codec for this entry type
    */
  def codec: Codec[ConstantPoolEntry] = rawCodec.withContext(getClass.getSimpleName)
}
object ConstantPoolTag {
  import ConstantPoolEntry._

  //https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.4-140
  case object Class              extends ConstantPoolTag(7, 1, uint16.as[ClassInfo].upcast)
  case object FieldRef           extends ConstantPoolTag(9, 1, (uint16 :: uint16).as[FieldRefInfo].upcast)
  case object MethodRef          extends ConstantPoolTag(10, 1, (uint16 :: uint16).as[MethodRefInfo].upcast)
  case object InterfaceMethodRef extends ConstantPoolTag(11, 1, (uint16 :: uint16).as[InterfaceMethodRefInfo].upcast)
  case object String             extends ConstantPoolTag(8, 1, uint16.as[StringInfo].upcast)
  case object Integer            extends ConstantPoolTag(3, 1, uint32.as[IntegerInfo].upcast)
  case object Float              extends ConstantPoolTag(4, 1, uint32.as[FloatInfo].upcast)
  case object Long               extends ConstantPoolTag(5, 2, (uint32 :: uint32).as[LongInfo].upcast)
  case object Double             extends ConstantPoolTag(6, 2, (uint32 :: uint32).as[DoubleInfo].upcast)
  case object NameAndType        extends ConstantPoolTag(12, 1, (uint16 :: uint16).as[NameAndTypeInfo].upcast)
  case object Utf8
      extends ConstantPoolTag(
        1,
        1,
        uint16.flatZip(bytes).xmap[ByteVector](_._2, b => b.length.toInt -> b).as[Utf8Info].upcast
      )
  case object MethodHandle  extends ConstantPoolTag(15, 1, (uint8 :: uint16).as[MethodHandleInfo].upcast)
  case object MethodType    extends ConstantPoolTag(16, 1, uint16.as[MethodTypeInfo].upcast)
  case object InvokeDynamic extends ConstantPoolTag(18, 1, (uint16 :: uint16).as[InvokeDynamicInfo].upcast)

  def tagForValue(value: Int): Option[ConstantPoolTag] = value match {
    case 7  => Some(Class)
    case 9  => Some(FieldRef)
    case 10 => Some(MethodRef)
    case 11 => Some(InterfaceMethodRef)
    case 8  => Some(String)
    case 3  => Some(Integer)
    case 4  => Some(Float)
    case 5  => Some(Long)
    case 6  => Some(Double)
    case 12 => Some(NameAndType)
    case 1  => Some(Utf8)
    case 15 => Some(MethodHandle)
    case 16 => Some(MethodType)
    case 18 => Some(InvokeDynamic)
    case _  => None
  }
}
