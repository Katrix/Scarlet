package scarlet.classfile.denormalized.attribute

import cats.data.Validated.Valid
import cats.data.ValidatedNel
import scarlet.classfile.denormalized.{AttributeOwner, ConstantPool}
import cats.syntax.all._
import scodec.bits.ByteVector
import scodec._
import scodec.codecs._

/**
  * An attribute that has not been parsed or exapnded, either because it's unknown,
  * or because it's companion wasn't specified when expanding attributes.
  * @param name The name of the attribute
  * @param data The raw data of the attribute
  */
case class Unknown(name: String, data: ByteVector) extends Attribute {
  private def decodeData[A](codec: Codec[A]) = codec.complete.decodeValue(data.bits).toEither.toValidatedNel

  override def expandUnknown(
      owner: AttributeOwner,
      constPool: ConstantPool,
      companions: Map[String, AttributeCompanion[_ <: Attribute]]
  ): ValidatedNel[Err, Attribute] =
    companions.get(name).map(comp => decodeData(comp.codec(owner, constPool, companions, name))).getOrElse(Valid(this))

  override type Self = Unknown
  override def companion: AttributeCompanion[Self] = Unknown
}
object Unknown extends AttributeCompanion[Unknown] {
  override def codec(
      owner: AttributeOwner,
      constPool: ConstantPool,
      companions: Map[String, AttributeCompanion[_ <: Attribute]],
      name: String
  ): Codec[Unknown] = bytes.xmap(Unknown(name, _), u => u.data)
}
