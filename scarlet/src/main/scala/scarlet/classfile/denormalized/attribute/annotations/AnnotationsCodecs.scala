package scarlet.classfile.denormalized.attribute.annotations

import scarlet.classfile.denormalized.ConstantPoolEntry._
import scarlet.classfile.denormalized.{ConstantPool, Descriptor}
import scarlet.classfile.denormalized.attribute.annotations.{Annotation => ScarletAnnotation}
import scodec._
import scodec.codecs._
import shapeless._

object AnnotationsCodecs {

  def fieldDescriptor(constPool: ConstantPool): Codec[Descriptor.FieldType] =
    constPool.constantUtf8Codec
      .narrow[Descriptor.FieldType](Descriptor.parseFieldStrAttempt, _.toStringDescriptor)

  def annotationCodec(constPool: ConstantPool): Codec[ScarletAnnotation] =
    (fieldDescriptor(constPool) :: vectorOfN(uint16, elementValuePairCodec(constPool))).as[ScarletAnnotation]

  def elementValuePairCodec(constPool: ConstantPool): Codec[ElementValuePair] =
    (constPool.constantUtf8Codec :: elementValueCodec(constPool)).as[ElementValuePair]

  def elementValueCodec(constPool: ConstantPool): Codec[ElementValue] = {
    def constantCodecPartial[P <: PoolValueConstant[PV]: Typeable, PV, EV, E <: ElementValue.ConstElementValue[EV]: Typeable](
        from: PV => EV,
        to: EV => PV
    )(
        implicit poolGen: Generic.Aux[P, PV :: HNil],
        elemGen: Generic.Aux[E, EV :: HNil]
    ): Codec[ElementValue] =
      constPool
        .constantCodec[P]
        .xmap[E](p => elemGen.from(from(p.value) :: HNil), e => poolGen.from(to(e.const) :: HNil))
        .upcast[ElementValue]

    def constantCodec[P <: PoolValueConstant[V]: Typeable, V, E <: ElementValue.ConstElementValue[V]: Typeable](
        implicit poolGen: Generic.Aux[P, V :: HNil],
        elemGen: Generic.Aux[E, V :: HNil]
    ) = constantCodecPartial[P, V, V, E](identity, identity)

    uint8
      .xmap[Char](_.toChar, _.toInt)
      .flatZip[ElementValue] {
        case 'B' => constantCodecPartial[IntegerInfo, Int, Byte, ElementValue.ByteValue](_.toByte, _.toInt)
        case 'C' => constantCodecPartial[IntegerInfo, Int, Char, ElementValue.CharValue](_.toChar, _.toInt)
        case 'D' => constantCodec[DoubleInfo, Double, ElementValue.DoubleValue]
        case 'F' => constantCodec[FloatInfo, Float, ElementValue.FloatValue]
        case 'I' => constantCodec[IntegerInfo, Int, ElementValue.IntValue]
        case 'J' => constantCodec[LongInfo, Long, ElementValue.LongValue]
        case 'S' => constantCodecPartial[IntegerInfo, Int, Short, ElementValue.ShortValue](_.toShort, _.toInt)
        case 'Z' => constantCodecPartial[IntegerInfo, Int, Boolean, ElementValue.BooleanValue](_ != 0, if (_) 1 else 0)
        case 's' => constPool.constantUtf8Codec.as[ElementValue.StringValue].upcast
        case 'e' => (fieldDescriptor(constPool) :: constPool.constantUtf8Codec).as[ElementValue.EnumValue].upcast
        case 'c' => constPool.constantCodec[ClassInfo].as[ElementValue.ClassValue].upcast
        case '@' => annotationCodec(constPool).as[ElementValue.AnnotationValue].upcast
        case '[' => vectorOfN(uint16, elementValueCodec(constPool)).as[ElementValue.ArrayValue].upcast
        case c   => fail[ElementValue](Err.MatchingDiscriminatorNotFound(c, Nil))
      }
      .xmap[ElementValue](_._2, v => v.tag -> v)
  }

  def typeAnnotationCodec(constPool: ConstantPool): Codec[TypeAnnotation] =
    (targetTypeCodec(constPool) :: typePathCodec(constPool) :: fieldDescriptor(constPool) :: vectorOfN(
      uint16,
      elementValuePairCodec(constPool)
    )).as[TypeAnnotation]

  def targetTypeCodec(constPool: ConstantPool): Codec[TargetType] = {
    import TargetType.{TargetInfo => Targets}
    val typeParameterTargetCodec      = uint8.as[Targets.TypeParameterTarget]
    val supertypeTargetCodec          = uint16.as[Targets.SupertypeTarget] //TODO: Need access to interfaces in ClassFile to resolve this
    val typeParameterBoundTargetCodec = (uint8 :: uint8).as[Targets.TypeParameterBoundTarget]
    val formalParameterTargetCodec    = uint8.as[Targets.FormalParameterTarget]
    val throwsTargetCodec             = uint16.as[Targets.ThrowsTarget] //TODO: Need access to Exceptions attribute
    val localVarTargetCodec =
      vectorOfN(uint16, (uint16 :: uint16 :: uint16).as[Targets.LocalVar]).as[Targets.LocalVarTarget]
    val catchTargetCodec        = uint16.as[Targets.CatchTarget] //TODO: Need access to Code attribute
    val offsetTargetCodec       = uint16.as[Targets.OffsetTarget]
    val typeArgumentTargetCodec = (uint16 :: uint8).as[Targets.TypeArgumentTarget]

    uint8
      .flatZip[TargetType] {
        case 0x00  => typeParameterTargetCodec.as[TargetType.ClassType].upcast
        case 0x01  => typeParameterTargetCodec.as[TargetType.MethodType].upcast
        case 0x10  => supertypeTargetCodec.as[TargetType.ExtendsType].upcast
        case 0x11  => typeParameterBoundTargetCodec.as[TargetType.ClassBoundsType].upcast
        case 0x12  => typeParameterBoundTargetCodec.as[TargetType.MethodBoundsType].upcast
        case 0x13  => provide(TargetType.FieldType).upcast
        case 0x14  => provide(TargetType.ReturnType).upcast
        case 0x15  => provide(TargetType.ReceiverTypeType).upcast
        case 0x16  => formalParameterTargetCodec.as[TargetType.FormalParameterType].upcast
        case 0x17  => throwsTargetCodec.as[TargetType.ThrowsType].upcast
        case 0x40  => localVarTargetCodec.as[TargetType.LocalVarType].upcast
        case 0x41  => localVarTargetCodec.as[TargetType.ResourceVarType].upcast
        case 0x42  => catchTargetCodec.as[TargetType.ExceptionParamType].upcast
        case 0x43  => offsetTargetCodec.as[TargetType.InstanceOfType].upcast
        case 0x44  => offsetTargetCodec.as[TargetType.NewType].upcast
        case 0x45  => offsetTargetCodec.as[TargetType.MethodRefNewType].upcast
        case 0x46  => offsetTargetCodec.as[TargetType.MethodRefType].upcast
        case 0x47  => typeArgumentTargetCodec.as[TargetType.CastType].upcast
        case 0x48  => typeArgumentTargetCodec.as[TargetType.NewGenericType].upcast
        case 0x49  => typeArgumentTargetCodec.as[TargetType.MethodGenericType].upcast
        case 0x4A  => typeArgumentTargetCodec.as[TargetType.MethodRefNewGenericType].upcast
        case 0x4B  => typeArgumentTargetCodec.as[TargetType.MethodRefGenericType].upcast
        case other => fail[TargetType](Err.MatchingDiscriminatorNotFound(other, Nil))
      }
      .xmap[TargetType](_._2, t => t.byteValue -> t)
  }

  def typePathCodec(pool: ConstantPool): Codec[TypePath] =
    vectorOfN(uint8, (uint8 :: uint8).as[TypePathElem]).as[TypePath]
}
