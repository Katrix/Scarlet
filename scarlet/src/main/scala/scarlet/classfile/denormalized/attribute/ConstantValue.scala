package scarlet.classfile.denormalized.attribute

import scarlet.classfile.denormalized.ConstantPoolEntry._
import scarlet.classfile.denormalized.{AttributeOwner, ConstantPool, Descriptor}
import scodec._
import scodec.codecs._
import shapeless._

sealed abstract class ConstantValue[A] extends NamedAttribute {
  def value: A

  type Self = ConstantValue[_]
  def companion: NamedAttributeCompanion[ConstantValue[_]] = ConstantValue
}
object ConstantValue extends NamedAttributeCompanion[ConstantValue[_]] {
  case class LongConst(value: Long)     extends ConstantValue[Long]
  case class FloatConst(value: Float)   extends ConstantValue[Float]
  case class DoubleConst(value: Double) extends ConstantValue[Double]
  case class IntegerConst(value: Int)   extends ConstantValue[Int]
  case class StringConst(value: String) extends ConstantValue[String]

  private def codecGen[P <: PoolValueConstant[V]: Typeable, V, A <: ConstantValue[V]](constPool: ConstantPool)(
      implicit poolGen: Generic.Aux[P, V :: HNil],
      attrGen: Generic.Aux[A, V :: HNil]
  ) = constPool.constantCodec[P].xmap[A](p => attrGen.from(p.value :: HNil), a => poolGen.from(a.value :: HNil))

  override def codec(
      owner: AttributeOwner,
      constPool: ConstantPool,
      companions: Map[String, AttributeCompanion[_ <: Attribute]],
      name: String
  ): Codec[ConstantValue[_]] = owner match {
    case AttributeOwner.FieldOwner(field) =>
      field.descriptor match {
        case Descriptor.BaseType.LongTpe   => codecGen[LongInfo, Long, ConstantValue.LongConst](constPool).upcast
        case Descriptor.BaseType.FloatTpe  => codecGen[FloatInfo, Float, ConstantValue.FloatConst](constPool).upcast
        case Descriptor.BaseType.DoubleTpe => codecGen[DoubleInfo, Double, ConstantValue.DoubleConst](constPool).upcast
        case Descriptor.BaseType.IntTpe | Descriptor.BaseType.ShortTpe | Descriptor.BaseType.CharTpe |
            Descriptor.BaseType.ByteTpe | Descriptor.BaseType.BooleanTpe =>
          codecGen[IntegerInfo, Int, ConstantValue.IntegerConst](constPool).upcast
        case Descriptor.ObjectType("java/lang/String") =>
          codecGen[StringInfo, String, ConstantValue.StringConst](constPool).upcast
        case _ => fail(Err(s"Found a ConstantValue attribute on a field with type ${field.descriptor}"))
      }
    case _ => fail(Err(s"Found a ConstantValue attribute on a ${owner.typeStr}"))
  }

  override def name: String = "ConstantValue"
}
