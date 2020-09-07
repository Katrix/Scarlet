package scarlet.classfile.denormalized

import cats.data.ValidatedNel
import cats.syntax.all._
import fastparse.NoWhitespace._
import fastparse._
import scarlet.classfile.MultiErr
import scodec.{Attempt, Err}

/**
  * A JVM descriptor. Used for fields and methods to describe types.
  */
sealed trait Descriptor {

  /**
    * Turns this descriptor back into it's string form.
    */
  def toStringDescriptor: String
}
object Descriptor {
  sealed trait ReturnType {
    def toStringDescriptor: String
  }

  sealed trait FieldType extends Descriptor with ReturnType

  sealed abstract class BaseType(val toStringDescriptor: String) extends FieldType
  object BaseType {
    case object ByteTpe    extends BaseType("B")
    case object CharTpe    extends BaseType("C")
    case object DoubleTpe  extends BaseType("D")
    case object FloatTpe   extends BaseType("F")
    case object IntTpe     extends BaseType("I")
    case object LongTpe    extends BaseType("J")
    case object ShortTpe   extends BaseType("S")
    case object BooleanTpe extends BaseType("Z")
  }
  case class ObjectType(className: String) extends FieldType {
    override def toStringDescriptor: String = s"L$className;"
  }
  case class ArrayType(tpe: FieldType) extends FieldType {
    override def toStringDescriptor: String = s"[${tpe.toStringDescriptor}"
  }

  case object VoidType extends ReturnType {
    override def toStringDescriptor: String = "V"
  }

  case class MethodDescriptor(paramTypes: Vector[FieldType], returnType: ReturnType) extends Descriptor {
    override def toStringDescriptor: String =
      s"(${paramTypes.map(_.toStringDescriptor).mkString})${returnType.toStringDescriptor}"
  }

  private def fieldDescriptor[_: P]: P[FieldType] = P(fieldType ~ End)

  private def fieldType[_: P]: P[FieldType] = P(baseType | objectType | arrayType)

  private def baseType[_: P]: P[BaseType] =
    P(
      P("B").map(_ => BaseType.ByteTpe) |
        P("C").map(_ => BaseType.CharTpe) |
        P("D").map(_ => BaseType.DoubleTpe) |
        P("F").map(_ => BaseType.FloatTpe) |
        P("I").map(_ => BaseType.IntTpe) |
        P("J").map(_ => BaseType.LongTpe) |
        P("S").map(_ => BaseType.ShortTpe) |
        P("Z").map(_ => BaseType.BooleanTpe)
    )

  private def objectType[_: P]: P[ObjectType] = P(P("L") ~ className ~ P(";")).map(ObjectType)

  private def arrayType[_: P]: P[ArrayType] = P(P("[") ~ componentType).map(ArrayType)

  private def componentType[_: P]: P[FieldType] = P(fieldType)

  private def className[_: P]: P[String] = P(CharsWhile(c => c != ';').!)

  private def methodDescriptor[_: P]: P[MethodDescriptor] =
    P("(" ~ parameterDescriptor.rep ~ ")" ~ returnDescriptor ~ End).map(t => MethodDescriptor(t._1.toVector, t._2))

  private def parameterDescriptor[_: P]: P[FieldType] = P(fieldType)

  private def returnDescriptor[_: P]: P[ReturnType] = P(fieldType | voidDescriptor)

  private def voidDescriptor[_: P]: P[VoidType.type] = P("V").map(_ => VoidType)

  private def descriptor[_: P]: P[Descriptor] = P(fieldDescriptor | methodDescriptor)

  private def parseEither[A](string: String, parser: P[_] => P[A]) = parse(string, parser) match {
    case Parsed.Success(value, _) => value.valid
    case failure: Parsed.Failure  => Err(failure.trace().msg).invalidNel
  }

  def parseStr(string: String): ValidatedNel[Err, Descriptor] =
    parseEither(string, descriptor(_))

  def parseFieldStr(string: String): ValidatedNel[Err, FieldType] =
    parseEither(string, fieldDescriptor(_))

  def parseFieldStrAttempt(string: String): Attempt[FieldType] =
    Attempt.fromEither(parseFieldStr(string).leftMap(es => MultiErr(es, Nil)).toEither)

  def parseMethodStr(string: String): ValidatedNel[Err, MethodDescriptor] =
    parseEither(string, methodDescriptor(_))
}
