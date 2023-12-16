package scarlet

import java.io.InputStream
import cats.data.Validated.Valid
import cats.data.{EitherNel, ValidatedNel, NonEmptyList => NEL}
import cats.syntax.all._
import fansi.Str
import org.apache.commons.text.StringEscapeUtils
import scalax.collection.GraphEdge._
import scalax.collection._
import scalax.collection.io.dot._
import scarlet.classfile.denormalized.attribute._
import scarlet.classfile.denormalized.opcodes.{OPCode => DeOPCode}
import scarlet.classfile.denormalized.{Classfile => DenormClassfile}
import scarlet.classfile.raw.{ClassfileCodec, Classfile => RawClassfile}
import scarlet.graph.{CFG, Structurer}
import scarlet.ir.OPCodeToSIR.StackFrame
import scarlet.ir._
import scodec.Err

import scala.collection.immutable.LongMap

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

  //Side path. Not used in further processing
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
          val codeVec = m.attributes.collect {
            case c: Code => c
          }

          val methodData = if (codeVec.size > 1) {
            Left(LongMap(0L -> NEL.one("Found more than one code attribute")))
          } else if (codeVec.size == 1) {
            val Vector(code: Code) = codeVec
            code.denormalizedCode.toEither.leftMap(_.map(t => t._1 -> t._2.map(_.messageWithContext)))
          } else {
            Right(LongMap.empty[DeOPCode])
          }

          MethodWithData(
            m.accessFlags,
            m.name,
            m.descriptor,
            m.attributes,
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

  private def cfgOPCodeDotEdgeTransformer[A](
      root: DotRootGraph,
      labelForNode: A => String
  )(innerEdge: Graph[A, DiEdge]#EdgeT): Option[(DotGraph, DotEdgeStmt)] = {
    innerEdge.edge match {
      case DiEdge(source, target) =>
        Some(
          (
            root,
            DotEdgeStmt(
              NodeId(labelForNode(source.value)),
              NodeId(labelForNode(target.value))
            )
          )
        )
    }
  }

  private def cfgNodeTransformer[A](
      root: DotRootGraph,
      nodeId: A => String,
      nodeStrings: A => Iterable[String]
  )(innerNode: Graph[A, DiEdge]#NodeT): Option[(DotGraph, DotNodeStmt)] =
    Some(
      (
        root,
        DotNodeStmt(
          NodeId(nodeId(innerNode.value)),
          Seq(
            DotAttr(
              Id("label"),
              Id(
                nodeStrings(innerNode.value).map(StringEscapeUtils.ESCAPE_HTML4.translate).mkString("<", "<BR />", ">")
              )
            )
          )
        )
      )
    )

  def sirBlockLabel(block: CFG.SIRBlock): String = block match {
    case CFG.SIRBlock.SIRCodeBasicBlock(leader, _)          => leader.toString
    case CFG.SIRBlock.SIRErrorBlock(_, _, _, codeWithStack) => codeWithStack.headOption.fold("<error>")(_._1.toString)
  }

  def formatErrorLines(map: LongMap[NEL[String]]): String = {
    map.toVector
      .flatMap {
        case (pc, NEL(error, Nil)) => Vector(s"$pc: $error")
        case (pc, NEL(headError, tailErrors)) =>
          val pcLength = pc.toString.length
          s"$pc: $headError" +: tailErrors.map(e => " " * pcLength + s": $e")
      }
      .mkString("\n")
  }

  object DenormalizedBytecodeCFG
      extends LanguageFunction[
        ClassfileWithData[Unit, Unit, LongMap[NEL[String]], LongMap[DeOPCode]],
        ClassfileWithData[Unit, Unit, LongMap[NEL[String]], CFG[CFG.OPCodeBasicBlock]]
      ] {
    override def process(
        in: ClassfileWithData[Unit, Unit, LongMap[NEL[String]], LongMap[DeOPCode]]
    ): EitherNel[String, ClassfileWithData[Unit, Unit, LongMap[NEL[String]], CFG[CFG.OPCodeBasicBlock]]] =
      Right(
        in.rightmapMethodWithMethod { (method, opCode) =>
          CFG.createFromOPCode(opCode, method.attributes.collectFirst { case code: Code => code })
        }
      )

    override def print(out: ClassfileWithData[Unit, Unit, LongMap[NEL[String]], CFG[CFG.OPCodeBasicBlock]]): Str = {
      val dotOut = out
        .rightmapMethodWithMethod { (method, cfg) =>
          val root = DotRootGraph(directed = true, Some(Id(method.name)))
          cfg.graph.toDot(
            root,
            edgeTransformer = cfgOPCodeDotEdgeTransformer[CFG.OPCodeBasicBlock](root, _.code.head._1.toString),
            cNodeTransformer = Some(
              cfgNodeTransformer[CFG.OPCodeBasicBlock](
                root,
                _.code.head._1.toString,
                _.code.map(t => s"${t._1}: ${t._2}")
              )
            )
          )
        }
        .leftmapMethod(formatErrorLines)

      Scarlet.printer(dotOut)
    }
  }

  object DenormalizedBytecodeCFGClassSyntax
      extends LanguageFunction[
        ClassfileWithData[Unit, Unit, LongMap[NEL[String]], CFG[CFG.OPCodeBasicBlock]],
        ClassfileWithData[Unit, Unit, LongMap[NEL[String]], CFG[CFG.OPCodeBasicBlock]]
      ] {
    override def process(
        in: ClassfileWithData[Unit, Unit, LongMap[NEL[String]], CFG[CFG.OPCodeBasicBlock]]
    ): EitherNel[String, ClassfileWithData[Unit, Unit, LongMap[NEL[String]], CFG[CFG.OPCodeBasicBlock]]] = Right(in)

    override def print(out: ClassfileWithData[Unit, Unit, LongMap[NEL[String]], CFG[CFG.OPCodeBasicBlock]]): Str = {
      val dotOut = out.rightmapMethodWithMethod { (method, cfg) =>
        val root = DotRootGraph(directed = true, Some(Id(method.name)))
        cfg.graph.toDot(
          root,
          edgeTransformer = cfgOPCodeDotEdgeTransformer[CFG.OPCodeBasicBlock](root, _.code.head._1.toString),
          cNodeTransformer = Some(
            cfgNodeTransformer[CFG.OPCodeBasicBlock](
              root,
              _.code.head._1.toString,
              _.code.map(t => s"${t._1}: ${t._2}")
            )
          )
        )
      }

      SimpleClassSyntax.classObjToSyntax(dotOut).syntax
    }
  }

  object `SIR-CFG`
      extends LanguageFunction[
        ClassfileWithData[Unit, Unit, LongMap[NEL[String]], CFG[CFG.OPCodeBasicBlock]],
        ClassfileWithData[
          Unit,
          Unit,
          LongMap[NEL[String]],
          CFG[CFG.SIRBlock]
        ]
      ] {
    override def process(
        in: ClassfileWithData[Unit, Unit, LongMap[NEL[String]], CFG[CFG.OPCodeBasicBlock]]
    ): EitherNel[String, ClassfileWithData[Unit, Unit, LongMap[NEL[String]], CFG[CFG.SIRBlock]]] =
      Right(in.rightmapMethod(OPCodeToSIR.convert))

    override def print(out: ClassfileWithData[Unit, Unit, LongMap[NEL[String]], CFG[CFG.SIRBlock]]): fansi.Str = {
      val dotOut = out
        .rightmapMethodWithMethod { (method, cfg) =>
          val root = DotRootGraph(directed = true, Some(Id(method.name)))

          cfg.graph.toDot(
            root,
            cfgOPCodeDotEdgeTransformer[CFG.SIRBlock](root, sirBlockLabel),
            cNodeTransformer = Some(
              cfgNodeTransformer(
                root,
                sirBlockLabel,
                {
                  case CFG.SIRBlock.SIRCodeBasicBlock(_, code) =>
                    code.toVector.flatMap {
                      case (pc, Vector(oneOp)) => Vector(s"$pc: $oneOp")
                      case (pc, Vector(headOp, tailOps @ _*)) =>
                        val pcLength = pc.toString.length
                        s"$pc: $headOp" +: tailOps.map(op => " " * pcLength + s": $op")
                    }
                  case CFG.SIRBlock.SIRErrorBlock(pc, error, op, codeWithStack) =>
                    codeWithStack.map {
                      case (pc, StackFrame(before, _, code, after)) =>
                        s"$pc: $code // Stack before: ${before.mkString(", ")} Stack after: ${after.mkString(", ")}"
                    }.toVector :+ s"$pc: $op //$error"
                }
              )
            )
          )
        }
        .leftmapMethod(formatErrorLines)

      Scarlet.printer(dotOut)
    }
  }

  def syntaxCode(pc: Long, code: Vector[SIR])(implicit syntaxExtra: SIR.SyntaxExtra): Vector[String] = {
    val lines = code.flatMap(SIR.toSyntax)
    lines match {
      case Vector()    => Vector()
      case Vector(one) => Vector(s"$pc: $one")
      case Vector(head, tail @ _*) =>
        s"$pc: $head" +: tail.toVector.map(line => " " * pc.toString.length + s": $line")
    }
  }

  def errorSyntaxCode(pc: Long, frame: StackFrame)(implicit syntaxExtra: SIR.SyntaxExtra): Vector[String] = {
    val lines       = SIR.toSyntax(frame.code)
    val stackBefore = s"Stack before: ${frame.before.mkString(", ")}"
    val stackAfter  = s" Stack after: ${frame.after.mkString(", ")}"
    lines match {
      case Nil        => Vector()
      case one :: Nil => Vector(s"$pc: $one //$stackBefore $stackAfter")
      case head :: tail =>
        s"// $stackBefore" +: s"$pc: $head" +: tail.toVector.map(line =>
          " " * pc.toString.length + s": $line"
        ) :+ s"// $stackAfter"
    }
  }

  object `SIR-CFG-ClassSyntax`
      extends LanguageFunction[
        ClassfileWithData[Unit, Unit, LongMap[NEL[String]], CFG[CFG.SIRBlock]],
        ClassfileWithData[Unit, Unit, LongMap[NEL[String]], CFG[CFG.SIRBlock]]
      ] {
    override def process(
        in: ClassfileWithData[Unit, Unit, LongMap[NEL[String]], CFG[CFG.SIRBlock]]
    ): EitherNel[String, ClassfileWithData[Unit, Unit, LongMap[NEL[String]], CFG[CFG.SIRBlock]]] = Right(in)

    override def print(out: ClassfileWithData[Unit, Unit, LongMap[NEL[String]], CFG[CFG.SIRBlock]]): Str = {
      val dotOut = out.rightmapMethodWithMethod { (method, cfg) =>
        val root                                  = DotRootGraph(directed = true, Some(Id(method.name)))
        implicit val syntaxExtra: SIR.SyntaxExtra = SIR.SyntaxExtra.fromAttributes(method.attributes)

        cfg.graph.toDot(
          root,
          cfgOPCodeDotEdgeTransformer[CFG.SIRBlock](root, sirBlockLabel),
          cNodeTransformer = Some(
            cfgNodeTransformer(
              root,
              sirBlockLabel,
              {
                case CFG.SIRBlock.SIRCodeBasicBlock(_, code) =>
                  SIR.toSyntaxBlockList(code)
                case CFG.SIRBlock.SIRErrorBlock(pc, error, op, codeWithStack) =>
                  codeWithStack.flatMap {
                    case (pc, frame) => errorSyntaxCode(pc, frame)
                  }.toVector :+ s"$pc: $op //$error"
              }
            )
          )
        )
      }

      SimpleClassSyntax.classObjToSyntax(dotOut).syntax
    }
  }

  type SIRClassSyntax = Either[Either[(DeOPCode, String), StackFrame], Vector[SIR]]
  object `SIR-ClassSyntax`
      extends LanguageFunction[ClassfileWithData[
        Unit,
        Unit,
        LongMap[NEL[String]],
        CFG[CFG.SIRBlock]
      ], ClassfileWithData[
        Unit,
        Unit,
        LongMap[NEL[String]],
        LongMap[SIRClassSyntax]
      ]] {
    override def process(
        in: ClassfileWithData[Unit, Unit, LongMap[NEL[String]], CFG[CFG.SIRBlock]]
    ): EitherNel[String, ClassfileWithData[Unit, Unit, LongMap[NEL[String]], LongMap[SIRClassSyntax]]] = {
      Right(
        in.rightmapMethod(cfg =>
          cfg.graph.nodes
            .map(_.value match {
              case CFG.SIRBlock.SIRCodeBasicBlock(_, code) => Right(code.to(LongMap))
              case CFG.SIRBlock.SIRErrorBlock(pc, error, op, codeWithStack) =>
                Left(
                  codeWithStack.transform((_, f) => Right(f)).updated(pc, Left((op, error)))
                )
            })
            .foldLeft(LongMap.empty[SIRClassSyntax]) {
              case (acc, Right(normalMap)) => acc ++ normalMap.transform((_, v) => Right(v))
              case (acc, Left(errorMap))   => acc ++ errorMap.transform((_, v) => Left(v))
            }
        )
      )
    }

    override def print(
        out: ClassfileWithData[Unit, Unit, LongMap[NEL[String]], LongMap[SIRClassSyntax]]
    ): Str = {
      val stringOut = out.rightmapMethodWithMethod { (method, sirCode) =>
        implicit val syntaxExtra: SIR.SyntaxExtra = SIR.SyntaxExtra.fromAttributes(method.attributes)

        sirCode.toVector.flatMap {
          case (pc, v) =>
            v match {
              case Right(code)             => SIR.toSyntaxPC(pc, code)
              case Left(Right(frame))      => errorSyntaxCode(pc, frame)
              case Left(Left((op, error))) => Vector(s"$pc: $op // $error")
            }
        }
      }

      SimpleClassSyntax.classObjToSyntax(stringOut).syntax
    }
  }

  object `SIR-Structured` extends LanguageFunction[ClassfileWithData[Unit, Unit, LongMap[NEL[String]], CFG[CFG.SIRBlock]], ClassfileWithData[Unit, Unit, LongMap[NEL[String]], CFG.SIRStructuredBlock]] {
    override def process(in: ClassfileWithData[Unit, Unit, LongMap[NEL[String]], CFG[CFG.SIRBlock]]): EitherNel[String, ClassfileWithData[Unit, Unit, LongMap[NEL[String]], CFG.SIRStructuredBlock]] =
      Right(in.rightmapMethod(cfg => Structurer.structureStart(cfg)))


    override def print(out: ClassfileWithData[Unit, Unit, LongMap[NEL[String]], CFG.SIRStructuredBlock]): Str = Scarlet.printer(out)
  }
}
