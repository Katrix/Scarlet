package scarlet

import java.io.InputStream

import cats.arrow.FunctionK

import scala.collection.immutable.LongMap
import cats.data.Validated.Valid
import cats.data.{EitherNel, ValidatedNel, NonEmptyList => NEL}
import cats.syntax.all._
import fansi.Str
import org.apache.commons.text.StringEscapeUtils
import scarlet.classfile.denormalized.attribute.{Code, MethodParameters, NamedAttributeCompanion}
import scarlet.classfile.denormalized.opcodes.{OPCode => DeOPCode}
import scarlet.classfile.denormalized.{Classfile => DenormClassfile}
import scarlet.classfile.raw.{ClassfileCodec, Classfile => RawClassfile}
import scarlet.graph.{CFG, OPCodeCFG}
import scarlet.ir.OPCodeToSIR.{StackFrame, CodeWithStack => SIRCode}
import scarlet.ir.{ClassfileWithData, FieldWithData, MethodWithData, OPCodeToSIR}
import scodec.Err

import scalax.collection._
import scalax.collection.GraphEdge._
import scalax.collection.io.dot._

trait LanguageFunction[I, O] { self =>

  def process(in: I): EitherNel[String, O]

  def print(out: O): fansi.Str

  def andThen[O2](next: LanguageFunction[O, O2]): LanguageFunction[I, O2] = new LanguageFunction[I, O2] {
    override def process(in: I): EitherNel[String, O2] = self.process(in).flatMap(next.process)

    override def print(out: O2): fansi.Str = next.print(out)
  }

  def andThenMapping[I2, O2](f: O => I2)(next: LanguageFunction[I2, O2]): LanguageFunction[I, O2] =
    new LanguageFunction[I, O2] {
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
      extends LanguageFunction[
        DenormClassfile,
        ClassfileWithData[Unit, Unit, LongMap[NEL[String]], LongMap[DeOPCode]]
      ] {
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
      Right(in.rightmapMethodWithMethod { (method, code) =>
        implicit val extra: ir.SIR.SyntaxExtra = ir.SIR.SyntaxExtra(method.attributes.collectFirst {
          case params: MethodParameters => params
        })
        code.map {
          case (k, StackFrame(_, _, v, _)) =>
            k -> v.flatMap(ir.SIR.toSyntax)
        }
      })
    }

    override def print(out: ClassfileWithData[Unit, Unit, NEL[String], LongMap[Vector[String]]]): fansi.Str =
      Scarlet.printer(
        out.rightmapMethod(code => code.values.flatten.mkString("\n"))
      )
  }

  private def cfgDotEdgeTransformer(
      root: DotRootGraph
  )(innerEdge: Graph[CFG.CodeBasicBlock, DiEdge]#EdgeT): Option[(DotGraph, DotEdgeStmt)] =
    innerEdge.edge match {
      case DiEdge(source, target) =>
        Some(
          (
            root,
            DotEdgeStmt(
              NodeId(source.value.code.head._1),
              NodeId(target.value.code.head._1)
            )
          )
        )
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
          def remapExprJump[B](expr: ir.SIR.Expr[B]): ir.SIR.Expr[B] = expr match {
            case ir.SIR.Expr.GetStackLocal(index, jumpTarget, tpe) =>
              ir.SIR.Expr.GetStackLocal(index, jumpTarget * 1000, tpe)
            case other => other.modifyChildren(FunctionK.lift(remapExprJump))
          }

          def remapJump(op: ir.SIR): ir.SIR = op match {
            case ir.SIR.SetStackLocal(index, pc, e) => ir.SIR.SetStackLocal(index, pc * 1000, remapExprJump(e))
            case ir.SIR.If(expr, branchPC)          => ir.SIR.If(remapExprJump(expr), branchPC * 1000)
            case ir.SIR.Switch(expr, defaultPC, pairs) =>
              ir.SIR.Switch(remapExprJump(expr), defaultPC * 1000, pairs.map(t => t._1 -> t._2 * 1000))
            case ir.SIR.Goto(branchPC) => ir.SIR.Goto(branchPC * 1000)
            case op                    => op.modifyExpr(FunctionK.lift(remapExprJump))
          }

          //TODO: Remap better
          val remaped =
            sirCode.flatMap(t1 => t1._2.code.zipWithIndex.map(t2 => (t1._1 * 1000 + t2._2) -> remapJump(t2._1)))

          CFG.createFromSIR(remaped)
        }
      )
    }

    override def print(
        out: ClassfileWithData[Unit, Unit, NEL[String], graph.CFG[graph.CFG.CodeBasicBlock]]
    ): fansi.Str = {
      val dotOut = out.rightmapMethod { cfg =>
        val root = DotRootGraph(directed = true, Some(Id("CodeCFG")))

        def nodeTransformer(innerNode: Graph[CFG.CodeBasicBlock, DiEdge]#NodeT): Option[(DotGraph, DotNodeStmt)] =
          Some(
            (
              root,
              DotNodeStmt(
                NodeId(innerNode.value.code.head._1),
                Seq(
                  DotAttr(
                    Id("label"),
                    Id(innerNode.value.code.map(t => s"${t._1}: ${t._2}").mkString("<", "<BR />", ">"))
                  )
                )
              )
            )
          )

        cfg.graph.toDot(root, cfgDotEdgeTransformer(root), cNodeTransformer = Some(nodeTransformer))
      }

      Scarlet.printer(dotOut)
    }
  }

  object `SIR-CFG-Syntax`
      extends LanguageFunction[
        ClassfileWithData[Unit, Unit, NEL[String], graph.CFG[graph.CFG.CodeBasicBlock]],
        ClassfileWithData[Unit, Unit, NEL[String], String]
      ] {
    override def process(
        in: ClassfileWithData[Unit, Unit, NEL[String], CFG[CFG.CodeBasicBlock]]
    ): EitherNel[String, ClassfileWithData[Unit, Unit, NEL[String], String]] =
      Right(
        in.rightmapMethodWithMethod { (method, cfg) =>
          val root = DotRootGraph(directed = true, Some(Id("CodeCFG")))

          implicit val extra: ir.SIR.SyntaxExtra = ir.SIR.SyntaxExtra(method.attributes.collectFirst {
            case params: MethodParameters => params
          })

          def nodeTransformer(innerNode: Graph[CFG.CodeBasicBlock, DiEdge]#NodeT): Option[(DotGraph, DotNodeStmt)] =
            Some(
              (
                root,
                DotNodeStmt(
                  NodeId(innerNode.value.code.head._1),
                  Seq(
                    DotAttr(
                      Id("label"),
                      Id(
                        innerNode.value.code
                          .flatMap(t => ir.SIR.toSyntax(t._2))
                          .map(StringEscapeUtils.escapeHtml4)
                          .mkString("<", "<BR />", ">")
                      )
                    ),
                    DotAttr(
                      Id("xlabel"),
                      Id(innerNode.value.code.head._1)
                    )
                  )
                )
              )
            )

          cfg.graph.toDot(root, cfgDotEdgeTransformer(root), cNodeTransformer = Some(nodeTransformer))
        }
      )

    override def print(out: ClassfileWithData[Unit, Unit, NEL[String], String]): Str =
      Scarlet.printer(out)
  }
}
