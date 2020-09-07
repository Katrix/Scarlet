package scarlet

import java.nio.file.Files

import scarlet.classfile.denormalized
import scarlet.opts.ScarletOptions
import org.apache.commons.text.StringEscapeUtils
import scodec.bits.{BitVector, ByteVector}
import scopt.OParser

object Scarlet {

  lazy val printer: pprint.PPrinter = pprint.PPrinter(
    120,
    Int.MaxValue,
    additionalHandlers = {
      case s: String     => pprint.Tree.Literal("\"" + StringEscapeUtils.escapeJava(s) + "\"")
      case b: BitVector  => pprint.Tree.Literal(s"0x${b.toHex}")
      case b: ByteVector => pprint.Tree.Literal(s"0x${b.toHex}")
      case s: denormalized.attribute.Signature.ParsedSignature =>
        pprint.Tree.Apply("Signature", Iterator(pprint.Tree.Literal(s.toStringSignature)))
      case d: denormalized.Descriptor =>
        pprint.Tree.Apply("Descriptor", Iterator(pprint.Tree.Literal(d.toStringDescriptor)))
    }
  )

  def main(args: Array[String]): Unit = {
    val options = OParser
      .parse(ScarletOptions.parser, args, ScarletOptions())
      .getOrElse(sys.exit(-1))

    def processAndPrint[I, O](language: LanguageFunction[I, O], in: I): Unit =
      language.process(in).map(language.print) match {
        case Left(es) =>
          Console.err.println("Encountered errors while processing class")
          es.toList.foreach(Console.err.println)
          sys.exit(-1)
        case Right(str) => println(str)
      }

    processAndPrint(options.language.langFunction, Files.newInputStream(options.inputFiles.head))
  }
}
