package scarlet.classfile.denormalized.attribute.annotations

import scarlet.classfile.denormalized.{AttributeOwner, ConstantPool}
import scarlet.classfile.denormalized.attribute.{Attribute, AttributeCompanion, NamedAttribute, NamedAttributeCompanion}
import scodec._
import scodec.codecs._

case class RuntimeInvisibleAnnotations(annotations: Vector[Annotation]) extends NamedAttribute {
  override type Self = RuntimeInvisibleAnnotations
  override def companion: NamedAttributeCompanion[Self] = RuntimeInvisibleAnnotations
}
object RuntimeInvisibleAnnotations extends NamedAttributeCompanion[RuntimeInvisibleAnnotations] {
  override def codec(
      owner: AttributeOwner,
      constPool: ConstantPool,
      companions: Map[String, AttributeCompanion[_ <: Attribute]],
      name: String
  ): Codec[RuntimeInvisibleAnnotations] =
    vectorOfN(uint16, AnnotationsCodecs.annotationCodec(constPool)).as[RuntimeInvisibleAnnotations]

  override def name: String = "RuntimeInvisibleAnnotations"
}

case class RuntimeInvisibleParameterAnnotations(parameterAnnotations: Vector[Vector[Annotation]])
    extends NamedAttribute {
  override type Self = RuntimeInvisibleParameterAnnotations
  override def companion: NamedAttributeCompanion[Self] = RuntimeInvisibleParameterAnnotations
}
object RuntimeInvisibleParameterAnnotations extends NamedAttributeCompanion[RuntimeInvisibleParameterAnnotations] {
  override def codec(
      owner: AttributeOwner,
      constPool: ConstantPool,
      companions: Map[String, AttributeCompanion[_ <: Attribute]],
      name: String
  ): Codec[RuntimeInvisibleParameterAnnotations] =
    vectorOfN(uint8, vectorOfN(uint16, AnnotationsCodecs.annotationCodec(constPool)))
      .as[RuntimeInvisibleParameterAnnotations]

  override def name: String = "RuntimeInvisibleParameterAnnotations"
}

case class RuntimeInvisibleTypeAnnotations(annotations: Vector[TypeAnnotation]) extends NamedAttribute {
  override type Self = RuntimeInvisibleTypeAnnotations
  override def companion: NamedAttributeCompanion[Self] = RuntimeInvisibleTypeAnnotations
}
object RuntimeInvisibleTypeAnnotations extends NamedAttributeCompanion[RuntimeInvisibleTypeAnnotations] {
  override def codec(
      owner: AttributeOwner,
      constPool: ConstantPool,
      companions: Map[String, AttributeCompanion[_ <: Attribute]],
      name: String
  ): Codec[RuntimeInvisibleTypeAnnotations] =
    vectorOfN(uint16, AnnotationsCodecs.typeAnnotationCodec(constPool)).as[RuntimeInvisibleTypeAnnotations]

  override def name: String = "RuntimeInvisibleTypeAnnotations"
}

case class RuntimeVisibleAnnotations(annotations: Vector[Annotation]) extends NamedAttribute {
  override type Self = RuntimeVisibleAnnotations
  override def companion: NamedAttributeCompanion[Self] = RuntimeVisibleAnnotations
}
object RuntimeVisibleAnnotations extends NamedAttributeCompanion[RuntimeVisibleAnnotations] {
  override def codec(
      owner: AttributeOwner,
      constPool: ConstantPool,
      companions: Map[String, AttributeCompanion[_ <: Attribute]],
      name: String
  ): Codec[RuntimeVisibleAnnotations] =
    vectorOfN(uint16, AnnotationsCodecs.annotationCodec(constPool)).as[RuntimeVisibleAnnotations]

  override def name: String = "RuntimeVisibleAnnotations"
}

case class RuntimeVisibleParameterAnnotations(parameterAnnotations: Vector[Vector[Annotation]]) extends NamedAttribute {
  override type Self = RuntimeVisibleParameterAnnotations
  override def companion: NamedAttributeCompanion[Self] = RuntimeVisibleParameterAnnotations
}
object RuntimeVisibleParameterAnnotations extends NamedAttributeCompanion[RuntimeVisibleParameterAnnotations] {
  override def codec(
      owner: AttributeOwner,
      constPool: ConstantPool,
      companions: Map[String, AttributeCompanion[_ <: Attribute]],
      name: String
  ): Codec[RuntimeVisibleParameterAnnotations] =
    vectorOfN(uint8, vectorOfN(uint16, AnnotationsCodecs.annotationCodec(constPool)))
      .as[RuntimeVisibleParameterAnnotations]

  override def name: String = "RuntimeVisibleParameterAnnotations"
}

case class RuntimeVisibleTypeAnnotations(annotations: Vector[TypeAnnotation]) extends NamedAttribute {
  override type Self = RuntimeVisibleTypeAnnotations
  override def companion: NamedAttributeCompanion[Self] = RuntimeVisibleTypeAnnotations
}
object RuntimeVisibleTypeAnnotations extends NamedAttributeCompanion[RuntimeVisibleTypeAnnotations] {
  override def codec(
      owner: AttributeOwner,
      constPool: ConstantPool,
      companions: Map[String, AttributeCompanion[_ <: Attribute]],
      name: String
  ): Codec[RuntimeVisibleTypeAnnotations] =
    vectorOfN(uint16, AnnotationsCodecs.typeAnnotationCodec(constPool)).as[RuntimeVisibleTypeAnnotations]

  override def name: String = "RuntimeVisibleTypeAnnotations"
}
