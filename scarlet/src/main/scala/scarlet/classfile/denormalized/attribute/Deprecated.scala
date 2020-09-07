package scarlet.classfile.denormalized.attribute

import scarlet.classfile.denormalized.{AttributeOwner, ConstantPool}
import scodec._
import scodec.codecs._

case object Deprecated extends NamedAttribute {
  override type Self = Deprecated.type
  override def companion: NamedAttributeCompanion[Self] = Companion

  object Companion extends NamedAttributeCompanion[Deprecated.type] {
    override def codec(
        owner: AttributeOwner,
        constPool: ConstantPool,
        companions: Map[String, AttributeCompanion[_ <: Attribute]],
        name: String
    ): Codec[Deprecated.type] = provide(Deprecated)

    override def name: String = "Deprecated"
  }
}
