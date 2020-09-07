package scarlet.classfile.denormalized.attribute

import scarlet.classfile.denormalized.{AttributeOwner, ConstantPool}
import scodec._
import scodec.codecs._

case object Synthetic extends NamedAttribute {
  override type Self = Synthetic.type
  override def companion: NamedAttributeCompanion[Self] = Companion

  object Companion extends NamedAttributeCompanion[Synthetic.type] {
    override def codec(
        owner: AttributeOwner,
        constPool: ConstantPool,
        companions: Map[String, AttributeCompanion[_ <: Attribute]],
        name: String
    ): Codec[Synthetic.type] = provide(Synthetic)

    override def name: String = "Synthetic"
  }
}
