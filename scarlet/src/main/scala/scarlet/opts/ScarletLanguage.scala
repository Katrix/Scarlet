package scarlet.opts

import java.io.InputStream

import scala.collection.immutable
import scala.collection.immutable.LongMap
import cats.data.{NonEmptyList => NEL}
import enumeratum.values._
import scarlet.classfile.denormalized.opcodes.{OPCode => DeOPCode}
import scarlet.classfile.denormalized.{Classfile => DenormClassfile}
import scarlet.classfile.raw.{Classfile => RawClassfile}
import scarlet.ir.{ClassfileWithData, SIR}
import scarlet.LanguageFunction
import scarlet.graph.CFG
import scarlet.ir.OPCodeToSIR.StackFrame

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

  case object BytecodeCFG
    extends ScarletLanguage[ClassfileWithData[Unit, Unit, LongMap[NEL[String]], CFG[CFG.OPCodeBasicBlock]]]("bytecode-cfg") {
    override def langFunction
    : LanguageFunction[InputStream, ClassfileWithData[Unit, Unit, LongMap[NEL[String]], CFG[CFG.OPCodeBasicBlock]]] =
      Bytecode.langFunction.andThen(LanguageFunction.DenormalizedBytecodeCFG)
  }

  case object BytecodeCFGClassSyntax
    extends ScarletLanguage[ClassfileWithData[Unit, Unit, LongMap[NEL[String]], CFG[CFG.OPCodeBasicBlock]]]("bytecode-cfg-classsyntax") {
    override def langFunction
    : LanguageFunction[InputStream, ClassfileWithData[Unit, Unit, LongMap[NEL[String]], CFG[CFG.OPCodeBasicBlock]]] =
      BytecodeCFG.langFunction.andThen(LanguageFunction.DenormalizedBytecodeCFGClassSyntax)
  }

  type SIROutput = ClassfileWithData[Unit, Unit, LongMap[NEL[String]], CFG[CFG.SIRBlock]]
  case object SIRCFG extends ScarletLanguage[SIROutput]("sir-cfg") {
    override def langFunction: LanguageFunction[InputStream, SIROutput] =
      BytecodeCFG.langFunction.andThen(LanguageFunction.`SIR-CFG`)
  }

  case object SIRCFGClassSyntax extends ScarletLanguage[SIROutput]("sir-cfg-classsyntax") {
    override def langFunction: LanguageFunction[InputStream, SIROutput] =
      SIRCFG.langFunction.andThen(LanguageFunction.`SIR-CFG-ClassSyntax`)
  }

  type SIRClassSyntax = Either[Either[(DeOPCode, String), StackFrame], Vector[SIR]]
  type SIRClassSyntaxOutput = ClassfileWithData[Unit, Unit, LongMap[NEL[String]], LongMap[SIRClassSyntax]]
  case object SIRClassSyntax extends ScarletLanguage[SIRClassSyntaxOutput]("sir-classsyntax") {
    override def langFunction: LanguageFunction[InputStream, SIRClassSyntaxOutput] =
      SIRCFG.langFunction.andThen(LanguageFunction.`SIR-ClassSyntax`)
  }

  type SIRStructuredOutput = ClassfileWithData[Unit, Unit, LongMap[NEL[String]], CFG.SIRStructuredBlock]
  case object SIRStrucutred extends ScarletLanguage[SIRStructuredOutput]("sir-structured") {
    override def langFunction: LanguageFunction[InputStream, SIRStructuredOutput] =
      SIRCFG.langFunction.andThen(LanguageFunction.`SIR-Structured`)
  }

  //case object TIR            extends ScarletLanguage
  //case object TIRSyntax      extends ScarletLanguage
  //case object Java           extends ScarletLanguage
  //case object Scala          extends ScarletLanguage

  override def values: immutable.IndexedSeq[ScarletLanguage[_]] = findValues

  implicit val scoptRead: scopt.Read[ScarletLanguage[_]] = scopt.Read.reads(ScarletLanguage.withValue)
}
