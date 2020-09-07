package scarlet.classfile.denormalized.attribute

import scarlet.classfile.denormalized.{AttributeOwner, ConstantPool}
import scarlet.classfile.denormalized.attribute.StackMapTable.StackMapFrame
import scodec.Codec

case class StackMapTable(stackMapFrame: Vector[StackMapFrame]) extends NamedAttribute {
  override type Self = StackMapTable
  override def companion: NamedAttributeCompanion[Self] = StackMapTable
}
object StackMapTable extends NamedAttributeCompanion[StackMapTable] {
  override def codec(
      owner: AttributeOwner,
      constPool: ConstantPool,
      companions: Map[String, AttributeCompanion[_ <: Attribute]],
      name: String
  ): Codec[StackMapTable] = ???
  case class StackMapFrame() //TODO

  override def name: String = "StackMapTable"
}
