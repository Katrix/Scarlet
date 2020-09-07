package scarlet.classfile.denormalized.attribute

import scarlet.classfile.denormalized.{AttributeOwner, ConstantPool, Descriptor}
import scarlet.classfile.denormalized.attribute.LocalVariableTable.LocalVariable
import scodec._
import scodec.codecs._

case class LocalVariableTable(localVariables: Vector[LocalVariable]) extends NamedAttribute {
  override type Self = LocalVariableTable
  override def companion: NamedAttributeCompanion[Self] = LocalVariableTable
}
object LocalVariableTable extends NamedAttributeCompanion[LocalVariableTable] {
  case class LocalVariable(startPc: Int, length: Int, name: String, descriptor: Descriptor.FieldType, index: Int)
  override def codec(
      owner: AttributeOwner,
      constPool: ConstantPool,
      companions: Map[String, AttributeCompanion[_ <: Attribute]],
      name: String
  ): Codec[LocalVariableTable] = {
    val localVariableCodec = (uint16 :: uint16 :: constPool.constantUtf8Codec :: constPool.constantUtf8Codec
      .narrow[Descriptor.FieldType](Descriptor.parseFieldStrAttempt, _.toStringDescriptor) :: uint16)
      .as[LocalVariable]

    vectorOfN(uint16, localVariableCodec).as[LocalVariableTable]
  }

  override def name: String = "LocalVariableTable"
}
