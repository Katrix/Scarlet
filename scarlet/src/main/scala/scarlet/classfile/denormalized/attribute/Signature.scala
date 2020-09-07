package scarlet.classfile.denormalized.attribute

import fastparse.NoWhitespace._
import fastparse._
import scarlet.classfile.denormalized.{AttributeOwner, ConstantPool}
import scodec._

case class Signature(signature: Signature.ParsedSignature) extends NamedAttribute {
  override type Self = Signature
  override def companion: NamedAttributeCompanion[Self] = Signature
}
object Signature extends NamedAttributeCompanion[Signature] {
  sealed trait ParsedSignature {
    def toStringSignature: String
  }

  case class ClassSignature(
      typeParameters: Option[TypeParameters],
      superclassSignature: ClassTypeSignature,
      superinterfaceSignature: Seq[ClassTypeSignature]
  ) extends ParsedSignature {
    override def toStringSignature: String =
      s"${typeParameters.fold("")(_.toStringSignature)}${superclassSignature.toStringSignature}${superinterfaceSignature.map(_.toStringSignature).mkString}"
  }
  case class MethodSignature(
      typeParameters: Option[TypeParameters],
      javaTypeSignatures: Seq[JavaTypeSignature],
      result: Result,
      throwsSignature: Seq[ThrowsSignature]
  ) extends ParsedSignature {
    override def toStringSignature: String =
      s"${typeParameters.fold("")(_.toStringSignature)}(${javaTypeSignatures
        .map(_.toStringSignature)
        .mkString})${result.toStringSignature}${throwsSignature.map(_.toStringSignature).mkString}"
  }
  sealed trait FieldSignature extends ParsedSignature

  sealed trait JavaTypeSignature extends Result {
    def toStringSignature: String
  }

  sealed trait ReferenceTypeSignature                           extends JavaTypeSignature with FieldSignature
  sealed abstract class BaseType(val toStringSignature: String) extends JavaTypeSignature
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

  case class ClassTypeSignature(
      packageSpecifier: Option[PackageSpecifier],
      simpleClassTypeSignature: SimpleClassTypeSignature,
      classTypeSignatureSuffix: Seq[ClassTypeSignatureSuffix]
  ) extends ReferenceTypeSignature {
    override def toStringSignature: String =
      s"L${packageSpecifier.fold("")(_.toStringSignature)}${simpleClassTypeSignature.toStringSignature}${classTypeSignatureSuffix.map(_.toStringSignature).mkString}"
  }
  case class TypeVariableSignature(identifier: String) extends ReferenceTypeSignature {
    override def toStringSignature: String = s"T$identifier;"
  }
  case class ArrayTypeSignature(javaTypeSignature: JavaTypeSignature) extends ReferenceTypeSignature {
    override def toStringSignature: String = s"[${javaTypeSignature.toStringSignature}"
  }

  case class PackageSpecifier(head: String, tail: Seq[PackageSpecifier]) {
    def toStringSignature: String = s"$head/${tail.map(_.toStringSignature).mkString}"
  }
  case class SimpleClassTypeSignature(identifier: String, typeArguments: Option[TypeArguments]) {
    def toStringSignature: String = s"$identifier${typeArguments.fold("")(_.toStringSignature)}"

  }
  case class ClassTypeSignatureSuffix(classTypeSignature: ClassTypeSignature) {
    def toStringSignature: String = s".${classTypeSignature.toStringSignature}"
  }

  case class TypeArguments(head: TypeArgument, tail: Seq[TypeArgument]) {
    def toStringSignature: String = s"<${head.toStringSignature}${tail.map(_.toStringSignature).mkString}>"
  }
  sealed trait TypeArgument {
    def toStringSignature: String
  }
  object TypeArgument {
    case object Wildcard extends TypeArgument {
      override def toStringSignature: String = "*"
    }
    case class Argument(wildcardIndicator: Option[WildcardIndicator], referenceTypeSignature: ReferenceTypeSignature)
        extends TypeArgument {
      override def toStringSignature: String =
        s"${wildcardIndicator.fold("")(_.toStringSignature)}${referenceTypeSignature.toStringSignature}"
    }
  }

  sealed trait WildcardIndicator {
    def toStringSignature: String
  }
  object WildcardIndicator {
    case object Plus extends WildcardIndicator {
      override def toStringSignature: String = "+"
    }
    case object Minus extends WildcardIndicator {
      override def toStringSignature: String = "-"
    }
  }

  case class TypeParameters(head: TypeParameter, tail: Seq[TypeParameter]) {
    def toStringSignature: String = s"<${head.toStringSignature}${tail.map(_.toStringSignature).mkString}>"
  }
  case class TypeParameter(
      identifier: String,
      classBound: Option[ReferenceTypeSignature],
      interfaceBound: Seq[ReferenceTypeSignature]
  ) {
    def toStringSignature: String =
      s"$identifier${classBound.fold("")(_.toStringSignature)}${interfaceBound.map(_.toStringSignature).mkString}"
  }

  sealed trait Result {
    def toStringSignature: String
  }
  object Result {
    object Void extends Result {
      override def toStringSignature: String = "V"
    }
  }

  sealed trait ThrowsSignature {
    def toStringSignature: String
  }
  object ThrowsSignature {
    case class ThrowClass(signature: ClassTypeSignature) extends ThrowsSignature {
      override def toStringSignature: String = s"^${signature.toStringSignature}"
    }
    case class ThrowTypeVar(signature: TypeVariableSignature) extends ThrowsSignature {
      override def toStringSignature: String = s"^${signature.toStringSignature}"
    }
  }

  private def javaTypeSignature[_: P]: P[JavaTypeSignature] = P(referenceTypeSignature | baseType)

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

  private def referenceTypeSignature[_: P]: P[ReferenceTypeSignature] =
    P(classTypeSignature | typeVariableSignature | arrayTypeSignature)

  private def classTypeSignature[_: P]: P[ClassTypeSignature] =
    P("L" ~ packageSpecifier.? ~ simpleClassTypeSignature ~ classTypeSignatureSuffix.rep ~ ";")
      .map(ClassTypeSignature.tupled)

  private def packageSpecifier[_: P]: P[PackageSpecifier] =
    P(identifier ~ "/" ~ packageSpecifier.rep).map(PackageSpecifier.tupled)

  private def simpleClassTypeSignature[_: P]: P[SimpleClassTypeSignature] =
    P(identifier ~ typeArguments.?).map(SimpleClassTypeSignature.tupled)

  private def typeArguments[_: P]: P[TypeArguments] =
    P("<" ~ typeArgument ~ typeArgument.rep ~ ">").map(TypeArguments.tupled)

  private def typeArgument[_: P]: P[TypeArgument] =
    P(
      (wildcardIndicator.? ~ referenceTypeSignature).map(TypeArgument.Argument.tupled) |
        P("*").map(_ => TypeArgument.Wildcard)
    )

  private def wildcardIndicator[_: P]: P[WildcardIndicator] =
    P(P("+").map(_ => WildcardIndicator.Plus) | P("-").map(_ => WildcardIndicator.Minus))

  private def classTypeSignatureSuffix[_: P]: P[ClassTypeSignatureSuffix] =
    P("." ~ classTypeSignature).map(ClassTypeSignatureSuffix)

  private def typeVariableSignature[_: P]: P[TypeVariableSignature] =
    P("T" ~ identifier ~ ";").map(TypeVariableSignature)

  private def arrayTypeSignature[_: P]: P[ArrayTypeSignature] = P("[" ~ javaTypeSignature).map(ArrayTypeSignature)

  private def identifier[_: P]: P[String] = P(CharsWhile(c => !".;[/<>:".contains(c)).!)

  private def classSignature[_: P]: P[ClassSignature] =
    P((typeParameters.? ~ superclassSignature ~ superinterfaceSignature.rep) ~ End).map(ClassSignature.tupled)

  private def typeParameters[_: P]: P[TypeParameters] =
    P("<" ~ typeParameter ~ typeParameter.rep ~ ">").map(TypeParameters.tupled)

  private def typeParameter[_: P]: P[TypeParameter] =
    P(identifier ~ classBound ~ interfaceBound.rep).map(TypeParameter.tupled)

  private def classBound[_: P]: P[Option[ReferenceTypeSignature]] = P(":" ~ referenceTypeSignature.?)

  private def interfaceBound[_: P]: P[ReferenceTypeSignature] = P(":" ~ referenceTypeSignature)

  private def superclassSignature[_: P]: P[ClassTypeSignature] = P(classTypeSignature)

  private def superinterfaceSignature[_: P]: P[ClassTypeSignature] = P(classTypeSignature)

  private def methodSignature[_: P]: P[MethodSignature] =
    P((typeParameters.? ~ "(" ~ javaTypeSignature.rep ~ ")" ~ result ~ throwsSignature.rep) ~ End).map(MethodSignature.tupled)

  private def result[_: P]: P[Result] = P(javaTypeSignature | P("V").map(_ => Result.Void))

  private def throwsSignature[_: P]: P[ThrowsSignature] =
    P(
      ("^" ~ classTypeSignature).map(ThrowsSignature.ThrowClass) |
        ("^" ~ typeVariableSignature).map(ThrowsSignature.ThrowTypeVar)
    )

  private def fieldSignature[_: P]: P[FieldSignature] = P(referenceTypeSignature ~ End)

  private def parseAttempt[A](string: String, parser: P[_] => P[A]) = parse(string, parser) match {
    case Parsed.Success(value, _) => Attempt.successful(value)
    case failure: Parsed.Failure      => Attempt.failure(Err(failure.trace().msg))
  }

  override def codec(
      owner: AttributeOwner,
      constPool: ConstantPool,
      companions: Map[String, AttributeCompanion[_ <: Attribute]],
      name: String
  ): Codec[Signature] = {
    def signatureCodec[A <: ParsedSignature](parser: P[_] => P[A]) =
      constPool.constantUtf8Codec.narrow[ParsedSignature](parseAttempt(_, parser), _.toStringSignature).as[Signature]
    owner match {
      case AttributeOwner.ClassOwner(_)  => signatureCodec(classSignature(_))
      case AttributeOwner.MethodOwner(_) => signatureCodec(methodSignature(_))
      case _                             => signatureCodec(fieldSignature(_))
    }
  }

  override def name: String = "Signature"
}
