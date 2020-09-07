package scarlet.classfile.denormalized.attribute.annotations
import scarlet.classfile.denormalized.{AttributeOwner, ConstantPool}
import scarlet.classfile.denormalized.attribute.{
  Attribute,
  AttributeCompanion,
  NamedAttribute,
  NamedAttributeCompanion
}
import scodec.Codec

case class AnnotationDefault(defaultValue: ElementValue) extends NamedAttribute {
  override type Self = AnnotationDefault
  override def companion: NamedAttributeCompanion[Self] = AnnotationDefault
}
object AnnotationDefault extends NamedAttributeCompanion[AnnotationDefault] {
  override def codec(
      owner: AttributeOwner,
      constPool: ConstantPool,
      companions: Map[String, AttributeCompanion[_ <: Attribute]],
      name: String
  ): Codec[AnnotationDefault] = AnnotationsCodecs.elementValueCodec(constPool).as[AnnotationDefault]
  override def name: String   = "AnnotationDefault"
}
