package scarlet.classfile.denormalized.attribute
import scarlet.classfile.denormalized.ConstantPoolEntry.ClassInfo
import scarlet.classfile.denormalized.{AttributeOwner, ConstantPool}
import scodec.codecs._
import scodec._

case class Exceptions(exceptionsThrown: Vector[ClassInfo]) extends NamedAttribute {
  override type Self = Exceptions
  override def companion: NamedAttributeCompanion[Self] = Exceptions
}
object Exceptions extends NamedAttributeCompanion[Exceptions] {
  override def codec(
      owner: AttributeOwner,
      constPool: ConstantPool,
      companions: Map[String, AttributeCompanion[_ <: Attribute]],
      name: String
  ): Codec[Exceptions] =
    vectorOfN(uint16, constPool.constantCodec[ClassInfo]).as[Exceptions]

  override def name: String = "Exceptions"
}
