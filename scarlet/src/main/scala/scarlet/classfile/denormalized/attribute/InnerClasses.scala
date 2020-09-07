package scarlet.classfile.denormalized.attribute

import scarlet.classfile.denormalized.ConstantPoolEntry.ClassInfo
import scarlet.classfile.denormalized.{AccessFlag, AttributeOwner, ConstantPool}
import scarlet.classfile.denormalized.attribute.InnerClasses.InnerClass
import scodec._
import scodec.codecs._

case class InnerClasses(classes: Vector[InnerClass]) extends NamedAttribute {
  override type Self = InnerClasses
  override def companion: NamedAttributeCompanion[Self] = InnerClasses
}
object InnerClasses extends NamedAttributeCompanion[InnerClasses] {
  case class InnerClass(
      innerClassInfo: ClassInfo,
      outerClassInfo: Option[ClassInfo],
      innerName: Option[String],
      innerClassAccessFlags: Set[AccessFlag]
  )

  override def codec(
      owner: AttributeOwner,
      constPool: ConstantPool,
      companions: Map[String, AttributeCompanion[_ <: Attribute]],
      name: String
  ): Codec[InnerClasses] = {
    val innerClassCodec = (constPool.constantCodec[ClassInfo] :: constPool
      .constantMaybeCodec[ClassInfo] :: constPool.constantMaybeUtf8Codec :: AccessFlag.codec).as[InnerClass]

    vectorOfN(uint16, innerClassCodec).as[InnerClasses]
  }

  override def name: String = "InnerClasses"
}
