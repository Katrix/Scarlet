package scarlet.classfile.denormalized.attribute

import scarlet.classfile.denormalized.{AttributeOwner, ConstantPool}
import scarlet.classfile.denormalized.ConstantPoolEntry.{ClassInfo, NameAndTypeInfo}
import scodec._

case class EnclosingMethod(clazz: ClassInfo, method: Option[NameAndTypeInfo]) extends NamedAttribute {
  override type Self = EnclosingMethod
  override def companion: NamedAttributeCompanion[Self] = EnclosingMethod
}
object EnclosingMethod extends NamedAttributeCompanion[EnclosingMethod] {
  override def codec(
      owner: AttributeOwner,
      constPool: ConstantPool,
      companions: Map[String, AttributeCompanion[_ <: Attribute]],
      name: String
  ): Codec[EnclosingMethod] =
    (constPool.constantCodec[ClassInfo] :: constPool.constantMaybeCodec[NameAndTypeInfo]).as[EnclosingMethod]

  override def name: String = "EnclosingMethod"
}
