package scarlet.classfile.denormalized.attribute

import scala.collection.immutable.LongMap

import cats.data.{NonEmptyList, Validated}
import cats.instances.either._
import cats.instances.vector._
import cats.syntax.all._
import scarlet.classfile.denormalized.AttributeOwner.CodeOwner
import scarlet.classfile.denormalized.ConstantPoolEntry.Utf8Info
import scarlet.classfile.denormalized.attribute.Code.ExceptionHandler
import scarlet.classfile.denormalized.{AttributeOwner, ConstantPool}
import scarlet.classfile.denormalized.opcodes.{OPCode => DeOPCode}
import scarlet.classfile.{MultiErr, raw}
import scarlet.classfile.raw.opcodes.{OPCodeCodec, OPCodeDenormalizer, OPCode => RawOPCode}
import scodec.codecs._
import scodec.{Attempt, Codec, Err}
import shapeless._

case class Code(
    maxStack: Int,
    maxLocals: Int,
    rawcode: LongMap[RawOPCode],
    denormalizedCode: Validated[LongMap[NonEmptyList[Err]], LongMap[DeOPCode]],
    exceptions: Vector[ExceptionHandler],
    attributes: Vector[Attribute]
) extends NamedAttribute {
  override type Self = Code
  override def companion: NamedAttributeCompanion[Self] = Code
}
object Code extends NamedAttributeCompanion[Code] {
  case class ExceptionHandler(startPc: Int, endPc: Int, handlerPc: Int, catchType: Int)

  override def codec(
      owner: AttributeOwner,
      constPool: ConstantPool,
      companions: Map[String, AttributeCompanion[_ <: Attribute]],
      name: String
  ): Codec[Code] = {
    val exception = (uint16 :: uint16 :: uint16 :: uint16).as[ExceptionHandler]
    val attributesCodec =
      raw.ClassfileCodec.attributes.narrow[Vector[Unknown]](
        rawAttr => {
          Attempt.fromEither(
            rawAttr.denormalize(constPool).leftMap(es => MultiErr(es, Nil)).toEither
          )
        },
        attrib =>
          raw.Attributes(attrib.map(attr => raw.AttributeInfo(constPool.indexOf(Utf8Info(attr.name)), attr.data)))
      )

    val codeCodec: Codec[LongMap[RawOPCode] :: Validated[LongMap[NonEmptyList[Err]], LongMap[DeOPCode]] :: HNil] =
      uint32
        .flatZip { length =>
          bytes(length.toInt).exmap[LongMap[RawOPCode]](
            vec => OPCodeCodec.opCodesCodec.decodeValue(vec.bits),
            vec => OPCodeCodec.opCodesCodec.encode(vec).map(_.bytes)
          )
        }
        .widen[LongMap[RawOPCode]](
          _._2,
          t => OPCodeCodec.opCodesCodec.encode(t).map(_.bytes.length -> t)
        )
        .xmap[LongMap[RawOPCode] :: Validated[LongMap[NonEmptyList[Err]], LongMap[DeOPCode]] :: HNil](
          v => {
            import cats.instances.vector._
            val validatedDenormalizedVec = v.toVector.traverse {
              case (address, op) =>
                OPCodeDenormalizer
                  .denormalize(op, address, constPool)
                  .bimap(es => NonEmptyList.one(address -> es), address -> _)
            }

            val addrSubst = LongMap(v.keys.zipWithIndex.map(t => t._1 -> t._2.toLong).toSeq: _*)

            val validatedDenormalized =
              validatedDenormalizedVec.bimap(
                { xs =>
                  val substitutedKeys = xs.map {
                    case (k, es) => addrSubst(k) -> es
                  }

                  LongMap(substitutedKeys.toList: _*)
                }, {
                  v =>
                    val substitutedKeys = v.map {
                      case (k, op) =>
                        import DeOPCode._
                        val substitutedOp = op match {
                          case IntIfZero(cond, branchAddress)    => IntIfZero(cond, addrSubst(branchAddress))
                          case IntIfCmp(cond, branchAddress) => IntIfCmp(cond, addrSubst(branchAddress))
                          case RefIf(cond, branchAddress)    => RefIf(cond, addrSubst(branchAddress))
                          case RefIfCmp(cond, branchAddress) => RefIfCmp(cond, addrSubst(branchAddress))
                          case Goto(branchAddress)           => Goto(addrSubst(branchAddress))
                          case Switch(defaultAddress, pairs) =>
                            Switch(defaultAddress, pairs.map(t => t._1 -> addrSubst(t._2)))
                          case _ => op
                        }
                        addrSubst(k) -> substitutedOp
                    }

                    LongMap(substitutedKeys: _*)
                }
              )

            v :: validatedDenormalized :: HNil
          },
          _.head
        )

    (uint16 :: uint16 :: codeCodec ::: vectorOfN(uint16, exception) :: attributesCodec.upcast[Vector[Attribute]])
      .as[Code]
      .exmap[Code](
        c => {
          Attempt.fromEither(
            c.attributes
              .traverse(_.expandUnknown(CodeOwner(c), constPool, companions))
              .map(attr => c.copy(attributes = attr))
              .leftMap(es => MultiErr(es, Nil))
              .toEither
          )
        },
        c => {
          Attempt.fromEither(
            c.attributes
              .traverse { a =>
                a.companion
                  .codec(CodeOwner(c), constPool, companions, a.name)
                  .encode(a.asInstanceOf[a.Self])
                  .toEither
                  .map(data => Unknown(a.name, data.bytes))
              }
              .map(unknownAttributes => c.copy(attributes = unknownAttributes))
          )
        }
      )
  }

  override def name: String = "Code"
}
