package scarlet.classfile.denormalized.attribute
import scarlet.classfile.denormalized.{AttributeOwner, ConstantPool}
import scodec._
import scodec.codecs._
import scodec.bits.ByteVector

case class SourceDebugExtension(debugExtension: ByteVector) extends NamedAttribute {
  override type Self = SourceDebugExtension
  override def companion: NamedAttributeCompanion[Self] = SourceDebugExtension
}
object SourceDebugExtension extends NamedAttributeCompanion[SourceDebugExtension] {
  override def codec(
      owner: AttributeOwner,
      constPool: ConstantPool,
      companions: Map[String, AttributeCompanion[_ <: Attribute]],
      name: String
  ): Codec[SourceDebugExtension] = bytes.as[SourceDebugExtension]

  override def name: String = "SourceDebugExtension"
}
