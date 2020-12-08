package scarlet.opts

import java.nio.file.{Files, Path, Paths}

import scopt.OParser
import scopt.Read

import cats.syntax.all._
import cats.instances.either._
import cats.instances.vector._

case class ScarletOptions(
    inputFiles: Seq[Path] = Nil,
    outputFolder: Option[Path] = None,
    language: ScarletLanguage[_] = ScarletLanguage.SIRClassSyntax
)
object ScarletOptions {
  val parser: OParser[Unit, ScarletOptions] = {
    val builder = OParser.builder[ScarletOptions]
    import builder._

    implicit val pathReads: Read[Path] = Read.reads(Paths.get(_))

    OParser.sequence(
      programName("scarlet"),
      head("scarlet", "0.1"),
      version('v', "version"),
      help('h', "help"),
      arg[Seq[Path]]("input")
        .unbounded()
        .text("The file to decompile")
        .action((f, o) => o.copy(inputFiles = f))
        .required()
        .validate { ps =>
          ps.toVector.traverse { p =>
            if (Files.notExists(p)) failure(s"The file represented by ${p.toString} does not exist")
            else success
          }.void
        },
      opt[ScarletLanguage[_]]('l', "lang")
        .text("The language to emit")
        .action((l, o) => o.copy(language = l))
        .required(),
      opt[Path]('o', "output")
        .text("Where to put the output of the decompilation")
        .action((p, o) => o.copy(outputFolder = Some(p)))
        .validate { p =>
          if (Files.notExists(p)) failure(s"The file represented by ${p.toString} does not exist")
          else success
        },
      checkConfig { o =>
        if ((o.inputFiles.size > 1 || !o.inputFiles.head.toString.endsWith(".class")) && o.outputFolder.isEmpty)
          failure("No output folder defined when multiple inputs specified, or when input is not a .class file")
        else success
      }
    )
  }
}
