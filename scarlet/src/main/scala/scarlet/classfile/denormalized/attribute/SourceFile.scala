package scarlet.classfile.denormalized.attribute

import scarlet.classfile.denormalized.{AttributeOwner, ConstantPool}
import scodec._

case class SourceFile(sourceFile: String) extends NamedAttribute {
  override type Self = SourceFile
  override def companion: NamedAttributeCompanion[Self] = SourceFile
}
object SourceFile extends NamedAttributeCompanion[SourceFile] {
  override def codec(
      owner: AttributeOwner,
      constPool: ConstantPool,
      companions: Map[String, AttributeCompanion[_ <: Attribute]],
      name: String
  ): Codec[SourceFile] = constPool.constantUtf8Codec.as[SourceFile]

  override def name: String = "SourceFile"
}
