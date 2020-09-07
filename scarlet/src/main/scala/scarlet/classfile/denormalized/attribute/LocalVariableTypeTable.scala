package scarlet.classfile.denormalized.attribute

import scarlet.classfile.denormalized.{AttributeOwner, ConstantPool, Descriptor}
import scarlet.classfile.denormalized.attribute.LocalVariableTypeTable.LocalVariableType
import scodec._
import scodec.codecs._

case class LocalVariableTypeTable(localVariableTypes: Vector[LocalVariableType]) extends NamedAttribute {
  override type Self = LocalVariableTypeTable
  override def companion: NamedAttributeCompanion[Self] = LocalVariableTypeTable
}
object LocalVariableTypeTable extends NamedAttributeCompanion[LocalVariableTypeTable] {
  case class LocalVariableType(startPc: Int, length: Int, name: String, signature: Descriptor.FieldType, index: Int)
  override def codec(
      owner: AttributeOwner,
      constPool: ConstantPool,
      companions: Map[String, AttributeCompanion[_ <: Attribute]],
      name: String
  ): Codec[LocalVariableTypeTable] = {
    val localVariableType =
      (uint16 :: uint16 :: constPool.constantUtf8Codec :: constPool.constantUtf8Codec
        .narrow[Descriptor.FieldType](Descriptor.parseFieldStrAttempt, _.toStringDescriptor) :: uint16)
        .as[LocalVariableType]

    vectorOfN(uint16, localVariableType).as[LocalVariableTypeTable]
  }

  override def name: String = "LocalVariableTypeTable"
}
