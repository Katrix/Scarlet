package scarlet.classfile.denormalized.attribute

import scarlet.classfile.denormalized.{AccessFlag, AttributeOwner, ConstantPool}
import scarlet.classfile.denormalized.attribute.MethodParameters.Parameter
import scodec._
import scodec.codecs._

case class MethodParameters(parameters: Vector[Parameter]) extends NamedAttribute {
  override type Self = MethodParameters
  override def companion: NamedAttributeCompanion[Self] = MethodParameters
}
object MethodParameters extends NamedAttributeCompanion[MethodParameters] {
  case class Parameter(name: Option[String], accessFlags: Set[AccessFlag])
  override def codec(
      owner: AttributeOwner,
      constPool: ConstantPool,
      companions: Map[String, AttributeCompanion[_ <: Attribute]],
      name: String
  ): Codec[MethodParameters] = {
    def parameterCodec = (constPool.constantMaybeUtf8Codec :: AccessFlag.codec).as[Parameter]

    vectorOfN(uint8, parameterCodec).as[MethodParameters]
  }

  override def name: String = "MethodParameters"
}
