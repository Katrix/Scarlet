package scarlet

import java.io.InputStream

import scala.collection.immutable.LongMap

import cats.data.Validated.Valid
import cats.data.{EitherNel, ValidatedNel, NonEmptyList => NEL}
import cats.syntax.all._
import scarlet.classfile.denormalized.attribute.{Code, NamedAttributeCompanion}
import scarlet.classfile.denormalized.opcodes.{OPCode => DeOPCode}
import scarlet.classfile.denormalized.{Classfile => DenormClassfile}
import scarlet.classfile.raw.{ClassfileCodec, Classfile => RawClassfile}
import scarlet.graph.{CFG, OPCodeCFG}
import scarlet.ir.OPCodeToSIR.{CodeWithStack => SIRCode}
import scarlet.ir.{ClassfileWithData, FieldWithData, MethodWithData, OPCodeToSIR}
import scodec.Err

trait LanguageFunction[I, O] { self =>

  def process(in: I): EitherNel[String, O]

  def print(out: O): fansi.Str

  def andThen[O2](next: LanguageFunction[O, O2]): LanguageFunction[I, O2] = new LanguageFunction[I, O2] {
    override def process(in: I): EitherNel[String, O2] = self.process(in).flatMap(next.process)

    override def print(out: O2): fansi.Str = next.print(out)
  }

  def andThenMapping[I2, O2](f: O => I2)(next: LanguageFunction[I2, O2]): LanguageFunction[I, O2] = new LanguageFunction[I, O2] {
    override def process(in: I): EitherNel[String, O2] = self.process(in).map(f).flatMap(next.process)

    override def print(out: O2): fansi.Str = next.print(out)
  }
}
object LanguageFunction {

  private def validatedErrToEither[A](validated: ValidatedNel[Err, A]): EitherNel[String, A] =
    validated.toEither.leftMap(_.map(_.messageWithContext))

  object RawClassfile extends LanguageFunction[InputStream, RawClassfile] {
    override def process(in: InputStream): EitherNel[String, RawClassfile] =
      validatedErrToEither(ClassfileCodec.parseClass(in).toEither.toValidatedNel)

    override def print(out: RawClassfile): fansi.Str = Scarlet.printer(out)
  }

  object DenormalizedClassfile extends LanguageFunction[RawClassfile, DenormClassfile] {
    override def process(in: RawClassfile): EitherNel[String, DenormClassfile] =
      validatedErrToEither(in.denormalize)

    override def print(out: DenormClassfile): fansi.Str = Scarlet.printer(out)
  }

  object RawBytecode extends LanguageFunction[DenormClassfile, DenormClassfile] {
    override def process(in: DenormClassfile): EitherNel[String, DenormClassfile] = {
      val expanded = validatedErrToEither(in.expandAttributes(NamedAttributeCompanion.defaults))

      //We remove the denormalized opcode so that we can focus on one thing
      expanded.map { classfile =>
        classfile.copy(methods = classfile.methods.map { method =>
          method.copy(attributes = method.attributes.map {
            case code: Code => code.copy(denormalizedCode = Valid(LongMap.empty))
            case other      => other
          })
        })
      }
    }

    override def print(out: DenormClassfile): fansi.Str = Scarlet.printer(out)
  }

  object DenormalizedBytecode
      extends LanguageFunction[DenormClassfile, ClassfileWithData[Unit, Unit, LongMap[NEL[String]], LongMap[DeOPCode]]] {
    override def process(
        in: DenormClassfile
    ): EitherNel[String, ClassfileWithData[Unit, Unit, LongMap[NEL[String]], LongMap[DeOPCode]]] = {
      val expanded = validatedErrToEither(in.expandAttributes(NamedAttributeCompanion.defaults))

      expanded.map { classfile =>
        val newMethods = classfile.methods.map { m =>
          val (codeVec, newAttributes) = m.attributes.partition {
            case _: Code => true
            case _       => false
          }

          val methodData = if (codeVec.size > 1) {
            Left[LongMap[NEL[String]], LongMap[DeOPCode]](
              LongMap(0L -> NEL.one("Found more than one code attribute"))
            )
          } else if (codeVec.size == 1) {
            val Vector(code: Code) = codeVec
            code.denormalizedCode.toEither.leftMap(_.map(t => t._1 -> t._2.map(_.messageWithContext)))
          } else {
            Right[LongMap[NEL[String]], LongMap[DeOPCode]](LongMap.empty[DeOPCode])
          }

          MethodWithData(
            m.accessFlags,
            m.name,
            m.descriptor,
            newAttributes,
            methodData
          )
        }

        ClassfileWithData(
          classfile.version,
          classfile.constantPool,
          classfile.accessFlags,
          classfile.thisClass,
          classfile.superClass,
          classfile.interfaces,
          classfile.fields.map { field =>
            FieldWithData(
              field.accessFlags,
              field.name,
              field.descriptor,
              field.attributes,
              Either.right[Unit, Unit](())
            )
          },
          newMethods,
          classfile.attributes
        )
      }
    }

    override def print(
        out: ClassfileWithData[Unit, Unit, LongMap[NEL[String]], LongMap[DeOPCode]]
    ): fansi.Str = Scarlet.printer(out)
  }

  object SIR
      extends LanguageFunction[ClassfileWithData[Unit, Unit, NEL[String], LongMap[DeOPCode]], ClassfileWithData[
        Unit,
        Unit,
        (NEL[String], SIRCode),
        SIRCode
      ]] {
    override def process(
        in: ClassfileWithData[Unit, Unit, NEL[String], LongMap[DeOPCode]]
    ): EitherNel[String, ClassfileWithData[Unit, Unit, (NEL[String], SIRCode), SIRCode]] = {
      Right(
        in.fullmapMethod {
          case Left(e)     => Either.left[(NEL[String], SIRCode), SIRCode]((e, LongMap.empty))
          case Right(data) => OPCodeToSIR.convert(data, OPCodeCFG.create(data)).leftMap(t => (NEL.one(t._1), t._2))
        }
      )
    }

    override def print(out: ClassfileWithData[Unit, Unit, (NEL[String], SIRCode), SIRCode]): fansi.Str =
      Scarlet.printer(out)
  }

  object `SIR-Syntax`
      extends LanguageFunction[ClassfileWithData[Unit, Unit, NEL[String], SIRCode], ClassfileWithData[
        Unit,
        Unit,
        NEL[String],
        LongMap[Vector[String]]
      ]] {
    override def process(
        in: ClassfileWithData[Unit, Unit, NEL[String], SIRCode]
    ): EitherNel[String, ClassfileWithData[Unit, Unit, NEL[String], LongMap[Vector[String]]]] = {
      Right(in.rightmapMethod { code =>
        code.map {
          case (k, (_, _, v, _)) =>
            val lines = v.flatMap {
              case ir.SIR.Nop                         => Nil
              case ir.SIR.MaybeInit(clazz)            => Seq(s"${clazz.name}.<clinit>")
              case ir.SIR.NotNull(expr)               => Seq(s"notNull(${expr.toSyntax})")
              case ir.SIR.NotZero(expr)               => Seq(s"notZero(${expr.toSyntax})")
              case ir.SIR.NotNegative(e)              => Seq(s"notNegative(${e.toSyntax})")
              case ir.SIR.SetLocal(index, e)          => Seq(s"var var_$index = ${e.toSyntax}")
              case ir.SIR.SetStackLocal(index, pc, e) => Seq(s"var stack_${index}_$pc = ${e.toSyntax}")
              case ir.SIR.SetArray(arr, idx, obj)     => Seq(s"${arr.toSyntax}[${idx.toSyntax}] = ${obj.toSyntax}")
              case ir.SIR.SetField(e, f, fieldRefInfo) =>
                Seq(s"${e.toSyntax}.${fieldRefInfo.nameAndType.name} = (${f.toSyntax})")
              case ir.SIR.SetStatic(fieldRefInfo, e) =>
                Seq(s"${fieldRefInfo.clazz.name}.${fieldRefInfo.nameAndType.name} = ${e.toSyntax}")
              case ir.SIR.New(varIndex, clazz, variables) =>
                Seq(s"var local_$varIndex = new ${clazz.name}(${variables.map(_.toSyntax).mkString(", ")})")
              case ir.SIR.CallSuper(e, variables) =>
                Seq(s"${e.toSyntax}.super(${variables.map(_.toSyntax).mkString(", ")})")
              case ir.SIR.Call(varIndex, _, clazz, name, _, None, variables) =>
                Seq(s"var local_$varIndex = ${clazz.name}.$name(${variables.map(_.toSyntax).mkString(", ")})")
              case ir.SIR.Call(varIndex, _, _, name, _, Some(obj), variables) =>
                Seq(s"var local_$varIndex = ${obj.toSyntax}.$name(${variables.map(_.toSyntax).mkString(", ")})")
              case ir.SIR.NewArray(varIndex, size, arrTpe) => Seq(s"var local_$varIndex = new ${arrTpe.describe}[${size.toSyntax}]")
              case ir.SIR.NewMultiArray(varIndex, tpe, sizesExpr) =>

                def underlyingTpe(tpe: ir.SIR.Type.Aux[_]): ir.SIR.Type = tpe match {
                  case ir.SIR.Type.Array(inner) => underlyingTpe(inner)
                  case _ => tpe
                }

                val dimensionsBrackets = sizesExpr.map(e => s"[${e.toSyntax}]").mkString

                Seq(s"var local_$varIndex = new ${underlyingTpe(tpe).describe}$dimensionsBrackets")
              case ir.SIR.If(expr, branchPC) => Seq(s"if(${expr.toSyntax}) goto $branchPC")
              case ir.SIR.Switch(expr, defaultPC, pairs) =>
                val gotos = pairs.map {
                  case (offset, pc) => s"case $offset: goto $pc"
                }

                s"switch(${expr.toSyntax}) {" +: gotos :+ s"default: goto $defaultPC" :+ "}"
              case ir.SIR.Goto(branchPC)     => Seq(s"goto $branchPC")
              case ir.SIR.Return(Some(expr)) => Seq(s"return ${expr.toSyntax}")
              case ir.SIR.Return(None)       => Seq("return")
              case ir.SIR.MonitorEnter(e)    => Seq(s"${e.toSyntax}.synchronizedStart")
              case ir.SIR.MonitorExit(e)     => Seq(s"${e.toSyntax}.synchronizedEnd")
            }

            k -> lines
        }

      })
    }

    override def print(out: ClassfileWithData[Unit, Unit, NEL[String], LongMap[Vector[String]]]): fansi.Str = {
      Scarlet.printer(
        out.rightmapMethod { code =>
          code.values.flatten.mkString("\n")
        }
      )
    }
  }

  object `SIR-CFG`
      extends LanguageFunction[ClassfileWithData[Unit, Unit, NEL[String], SIRCode], ClassfileWithData[
        Unit,
        Unit,
        NEL[String],
        graph.CFG[graph.CFG.CodeBasicBlock]
      ]] {
    override def process(
        in: ClassfileWithData[Unit, Unit, NEL[String], SIRCode]
    ): EitherNel[String, ClassfileWithData[Unit, Unit, NEL[String], graph.CFG[graph.CFG.CodeBasicBlock]]] = {
      Right(
        in.rightmapMethod { sirCode =>
          val redistributed = ???

          CFG.createFromSIR(redistributed)
        }
      )
    }

    override def print(
        out: ClassfileWithData[Unit, Unit, NEL[String], graph.CFG[graph.CFG.CodeBasicBlock]]
    ): fansi.Str =
      Scarlet.printer(out)
  }
}
