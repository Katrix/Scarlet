package scarlet.classfile.denormalized.attribute

import cats.data.Validated.Valid
import cats.data.ValidatedNel
import scarlet.classfile.denormalized.{AttributeOwner, ConstantPool}
import scodec.{Codec, Err}

abstract class Attribute {
  type Self <: Attribute
  def name: String
  def companion: AttributeCompanion[Self]

  def expandUnknown(
      owner: AttributeOwner,
      constPool: ConstantPool,
      companions: Map[String, AttributeCompanion[_ <: Attribute]]
  ): ValidatedNel[Err, Attribute] = Valid(this)
}
abstract class NamedAttribute extends Attribute {
  def name: String = companion.name

  override def companion: NamedAttributeCompanion[Self]
}

trait AttributeCompanion[A <: Attribute] {
  def codec(
      owner: AttributeOwner,
      constPool: ConstantPool,
      companions: Map[String, AttributeCompanion[_ <: Attribute]],
      name: String
  ): Codec[A]
}
trait NamedAttributeCompanion[A <: Attribute] extends AttributeCompanion[A] {
  def name: String
}
object NamedAttributeCompanion {

  val defaults: Seq[NamedAttributeCompanion[_ <: Attribute]] = Seq(
    annotations.AnnotationDefault,
    annotations.RuntimeInvisibleAnnotations,
    annotations.RuntimeVisibleAnnotations,
    annotations.RuntimeInvisibleParameterAnnotations,
    annotations.RuntimeVisibleParameterAnnotations,
    annotations.RuntimeInvisibleTypeAnnotations,
    annotations.RuntimeVisibleTypeAnnotations,
    BootstrapMethods,
    Code,
    ConstantValue,
    Deprecated.Companion,
    EnclosingMethod,
    Exceptions,
    InnerClasses,
    LineNumberTable,
    LocalVariableTable,
    LocalVariableTypeTable,
    MethodParameters,
    Signature,
    SourceDebugExtension,
    SourceFile,
    //StackMapTable,
    Synthetic.Companion
  )
}
