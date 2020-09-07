package scarlet.classfile.denormalized.attribute

import scarlet.classfile.denormalized.{AttributeOwner, ConstantPool}
import scarlet.classfile.denormalized.attribute.LineNumberTable.LineNumber
import scodec._
import scodec.codecs._

case class LineNumberTable(lineNumbers: Vector[LineNumber]) extends NamedAttribute {
  override type Self = LineNumberTable
  override def companion: NamedAttributeCompanion[Self] = LineNumberTable
}
object LineNumberTable extends NamedAttributeCompanion[LineNumberTable] {
  case class LineNumber(startPc: Int, lineNumber: Int)
  override def codec(
      owner: AttributeOwner,
      constPool: ConstantPool,
      companions: Map[String, AttributeCompanion[_ <: Attribute]],
      name: String
  ): Codec[LineNumberTable] = {
    val lineNumberCodec = (uint16 :: uint16).as[LineNumber]
    vectorOfN(uint16, lineNumberCodec).as[LineNumberTable]
  }

  override def name: String = "LineNumberTable"
}
