package scarlet.opts

import java.io.InputStream

import scala.collection.immutable
import scala.collection.immutable.LongMap

import cats.data.{NonEmptyList => NEL}
import enumeratum.values._
import scarlet.classfile.denormalized.opcodes.{OPCode => DeOPCode}
import scarlet.classfile.denormalized.{Classfile => DenormClassfile}
import scarlet.classfile.raw.{Classfile => RawClassfile}
import scarlet.ir.ClassfileWithData
import scarlet.ir.OPCodeToSIR.{CodeWithStack => SIRCode}
import scarlet.{LanguageFunction, graph}

sealed abstract class ScarletLanguage[A](val value: String) extends StringEnumEntry {

  def langFunction: LanguageFunction[InputStream, A]
}
object ScarletLanguage extends StringEnum[ScarletLanguage[_]] {
  case object RawClassfile extends ScarletLanguage[RawClassfile]("raw-classfile") {
    override def langFunction: LanguageFunction[InputStream, RawClassfile] =
      LanguageFunction.RawClassfile
  }
  case object Classfile extends ScarletLanguage[DenormClassfile]("classfile") {
    override def langFunction: LanguageFunction[InputStream, DenormClassfile] =
      RawClassfile.langFunction.andThen(LanguageFunction.DenormalizedClassfile)
  }
  case object RawBytecode extends ScarletLanguage[DenormClassfile]("raw-bytecode") {
    override def langFunction: LanguageFunction[InputStream, DenormClassfile] =
      Classfile.langFunction.andThen(LanguageFunction.RawBytecode)
  }
  //Raw bytecode throws away the denormalized bytecode, so we go one more step back
  case object Bytecode
      extends ScarletLanguage[ClassfileWithData[Unit, Unit, LongMap[NEL[String]], LongMap[DeOPCode]]]("bytecode") {
    override def langFunction
        : LanguageFunction[InputStream, ClassfileWithData[Unit, Unit, LongMap[NEL[String]], LongMap[DeOPCode]]] =
      Classfile.langFunction.andThen(LanguageFunction.DenormalizedBytecode)
  }
  //case object BytecodeSyntax extends ScarletLanguage

  type SIROutput = ClassfileWithData[Unit, Unit, (NEL[String], SIRCode), SIRCode]
  case object SIR extends ScarletLanguage[SIROutput]("sir") {
    override def langFunction: LanguageFunction[InputStream, SIROutput] =
      Bytecode.langFunction.andThenMapping(
        _.leftmapMethod(es => NEL.fromListUnsafe(es.values.flatMap(_.toList).toList))
      )(LanguageFunction.SIR)
  }

  type SIRSyntaxOutput = ClassfileWithData[Unit, Unit, NEL[String], LongMap[Vector[String]]]
  case object SIRSyntax extends ScarletLanguage[SIRSyntaxOutput]("sir-syntax") {
    override def langFunction: LanguageFunction[InputStream, SIRSyntaxOutput] =
      SIR.langFunction.andThenMapping(_.leftmapMethod(_._1))(LanguageFunction.`SIR-Syntax`)
  }
  type StringOutput = ClassfileWithData[Unit, Unit, NEL[String], String]
  case object SIRClassSyntax extends ScarletLanguage[StringOutput]("sir-classsyntax") {
    override def langFunction: LanguageFunction[InputStream, StringOutput] =
      SIRSyntax.langFunction.andThen(LanguageFunction.`SIR-ClassSyntax`)
  }

  type SIRCFGOutput = ClassfileWithData[Unit, Unit, NEL[String], graph.CFG[graph.CFG.CodeBasicBlock]]
  case object SIRCFG extends ScarletLanguage[SIRCFGOutput]("sir-cfg") {
    override def langFunction: LanguageFunction[InputStream, SIRCFGOutput] =
      SIR.langFunction.andThenMapping(_.leftmapMethod(_._1))(LanguageFunction.`SIR-CFG`)
  }
  case object SIRCFGSyntax extends ScarletLanguage[StringOutput]("sir-cfg-syntax") {
    override def langFunction: LanguageFunction[InputStream, StringOutput] =
      SIRCFG.langFunction.andThen(LanguageFunction.`SIR-CFG-Syntax`)
  }
  case object SIRCFGClassSyntax extends ScarletLanguage[StringOutput]("sir-cfg-classsyntax") {
    override def langFunction: LanguageFunction[InputStream, StringOutput] =
      SIRCFGSyntax.langFunction.andThen(LanguageFunction.`SIR-CFG-ClassSyntax`)
  }
  //case object TIR            extends ScarletLanguage
  //case object TIRSyntax      extends ScarletLanguage
  //case object Java           extends ScarletLanguage
  //case object Scala          extends ScarletLanguage

  override def values: immutable.IndexedSeq[ScarletLanguage[_]] = findValues

  implicit val scoptRead: scopt.Read[ScarletLanguage[_]] = scopt.Read.reads(ScarletLanguage.withValue)
}
