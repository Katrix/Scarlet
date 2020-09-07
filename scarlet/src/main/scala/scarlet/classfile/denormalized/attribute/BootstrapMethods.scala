package scarlet.classfile.denormalized.attribute

import scarlet.classfile.denormalized.{AttributeOwner, ConstantPool, ConstantPoolEntry}
import scarlet.classfile.denormalized.ConstantPoolEntry._
import scarlet.classfile.denormalized.attribute.BootstrapMethods.BootstrapMethod
import scodec.codecs._
import scodec._
import shapeless._
import cats.syntax.all._
import shapeless.ops.coproduct.Inject

case class BootstrapMethods(bootstrapMethod: Vector[BootstrapMethod]) extends NamedAttribute {
  override type Self = BootstrapMethods
  override def companion: NamedAttributeCompanion[Self] = BootstrapMethods
}
object BootstrapMethods extends NamedAttributeCompanion[BootstrapMethods] {
  type BootstrapArgument =
    StringInfo :+: IntegerInfo :+: FloatInfo :+: LongInfo :+: DoubleInfo :+: ClassInfo :+: MethodHandleInfo :+: MethodTypeInfo :+: CNil
  case class BootstrapMethod(
      bootstrapMethodRef: MethodHandleInfo,
      bootstrapArguments: Vector[BootstrapArgument]
  )
  override def codec(
      owner: AttributeOwner,
      constPool: ConstantPool,
      companions: Map[String, AttributeCompanion[_ <: Attribute]],
      name: String
  ): Codec[BootstrapMethods] = {
    def bootstrapMethodCodec =
      (constPool.constantCodec[MethodHandleInfo] :: vectorOfN(
        uint16,
        uint16.narrow[BootstrapArgument](
          { i =>
            val create =
              Coproduct[BootstrapArgument]

            def tryType[A <: ConstantPoolEntry: Typeable](implicit inj: Inject[BootstrapArgument, A]) =
              constPool.getAttempt[A](i).map(create(_))

            tryType[StringInfo]
              .orElse(tryType[IntegerInfo])
              .orElse(tryType[FloatInfo])
              .orElse(tryType[LongInfo])
              .orElse(tryType[DoubleInfo])
              .orElse(tryType[ClassInfo])
              .orElse(tryType[MethodHandleInfo])
              .orElse(tryType[MethodTypeInfo])
          },
          a => constPool.indexOf(a.unify)
        )
      )).as[BootstrapMethod]

    vectorOfN(uint16, bootstrapMethodCodec).as[BootstrapMethods]
  }

  override def name: String = "BootstrapMethods"
}
