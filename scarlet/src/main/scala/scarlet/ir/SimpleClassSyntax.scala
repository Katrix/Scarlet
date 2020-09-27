package scarlet.ir

import cats.data.{NonEmptyList => NEL}
import org.apache.commons.text.StringEscapeUtils
import scarlet.classfile.denormalized.ConstantPoolEntry.ClassInfo
import scarlet.classfile.denormalized.attribute._
import scarlet.classfile.denormalized.attribute.annotations._
import scarlet.classfile.denormalized.{AccessFlag, Descriptor}

import scala.meta._

object SimpleClassSyntax {

  def accessFlagsToMods(flags: Set[AccessFlag]): Set[Mod] = flags.flatMap {
    case AccessFlag.Public => None
    //Classfiles don't have private and protected scopes
    case AccessFlag.Private   => Some(Mod.Private(Name.Anonymous()))
    case AccessFlag.Protected => Some(Mod.Protected(Name.Anonymous()))
    case AccessFlag.Static    => Some(mod"@scala.static")
    case AccessFlag.Final     => Some(Mod.Final())
    case AccessFlag.Volatile  => Some(mod"@scala.volatile")
    case AccessFlag.Transient => Some(mod"@scala.transient")
    case AccessFlag.Synthetic => Some(mod"@synthetic")
    case AccessFlag.Enum      => Some(mod"@enum")
  }

  def fieldDescriptorToType(descriptor: Descriptor.FieldType): Type = descriptor match {
    case Descriptor.BaseType.ByteTpe    => t"scala.Byte"
    case Descriptor.BaseType.CharTpe    => t"scala.Char"
    case Descriptor.BaseType.DoubleTpe  => t"scala.Double"
    case Descriptor.BaseType.FloatTpe   => t"scala.Float"
    case Descriptor.BaseType.IntTpe     => t"scala.Int"
    case Descriptor.BaseType.LongTpe    => t"scala.Long"
    case Descriptor.BaseType.ShortTpe   => t"scala.Short"
    case Descriptor.BaseType.BooleanTpe => t"scala.Boolean"
    case Descriptor.ObjectType(className) =>
      val parts       = className.split("/")
      val packagePart = parts.init
      val classType   = Type.Name(parts.last)

      packageTermRef(packagePart.toList).fold(classType: Type)(Type.Select(_, classType))
    case Descriptor.ArrayType(tpe) => t"scala.Array[${fieldDescriptorToType(tpe)}]"
  }

  def typeToTerm(tpe: Type): Term = tpe match {
    case Type.Name(name)                   => Term.Name(name)
    case Type.Select(ref, Type.Name(name)) => Term.Select(ref, Term.Name(name))
    case _                                 => sys.error("Tried to convert unsupported Type to Term")
  }

  def elementValueToLit(value: ElementValue): Term = value match {
    case ElementValue.ByteValue(const)    => Lit.Byte(const)
    case ElementValue.CharValue(const)    => Lit.Char(const)
    case ElementValue.DoubleValue(const)  => Lit.Double(const)
    case ElementValue.FloatValue(const)   => Lit.Float(const)
    case ElementValue.IntValue(const)     => Lit.Int(const)
    case ElementValue.LongValue(const)    => Lit.Long(const)
    case ElementValue.ShortValue(const)   => Lit.Short(const)
    case ElementValue.BooleanValue(const) => Lit.Boolean(const)
    case ElementValue.StringValue(const)  => Lit.String(StringEscapeUtils.escapeJava(const))
    case ElementValue.EnumValue(typeName, constName) =>
      Term.Select(typeToTerm(fieldDescriptorToType(typeName)), Term.Name(constName))

    case ElementValue.ClassValue(classInfo)       => q"scala.Predef.classOf[${classInfoToType(classInfo)}]"
    case ElementValue.AnnotationValue(annotation) => Term.New(annotationToMod(annotation).init)
    case ElementValue.ArrayValue(values)          => q"scala.Array(..${values.map(elementValueToLit).toList})"
  }

  def annotationToMod(annotation: Annotation): Mod.Annot =
    Mod.Annot(
      Init(
        fieldDescriptorToType(annotation.fieldType),
        Name.Anonymous(),
        List(annotation.elementValuePairs.map {
          case ElementValuePair(name, value) => Term.Assign(Term.Name(name), elementValueToLit(value))
        }.toList)
      )
    )

  def annotationsFromAttributes(attributes: Seq[Attribute]): Seq[Mod.Annot] = attributes.collect {
    case RuntimeVisibleAnnotations(annotations)   => annotations.map(annotationToMod)
    case RuntimeInvisibleAnnotations(annotations) => annotations.map(annotationToMod)
  }.flatten

  def paramterAnnotationsFromAttributes(attributes: Seq[Attribute]): Option[Vector[Vector[Mod.Annot]]] =
    attributes.collectFirst {
      case RuntimeVisibleParameterAnnotations(annotations)   => annotations.map(_.map(annotationToMod))
      case RuntimeInvisibleParameterAnnotations(annotations) => annotations.map(_.map(annotationToMod))
    }

  def packageTermRef(packages: List[String]): Option[Term.Ref] =
    Option.when(packages.nonEmpty)(
      packages.tail.foldLeft(Term.Name(packages.head): Term.Ref)((acc, s) => Term.Select(acc, Term.Name(s)))
    )

  def typeParameterToType(typeParameter: Signature.TypeParameter): Type.Param = {
    val bounds           = typeParameter.classBound.toSeq ++ typeParameter.interfaceBound.toSeq
    lazy val boundsTypes = bounds.map(signatureToType)
    lazy val boundType   = boundsTypes.reduce(Type.And(_, _))

    Type.Param(
      Nil, //TODO: Support type param annotations here. Scala doesn't support them, but might as well see if Scalameta does
      Type.Name(typeParameter.identifier),
      Nil, //Classfiles don't support HKT
      Type.Bounds(
        None, //Classfiles doesn't support lower bounds here
        Option.when(bounds.nonEmpty)(boundType)
      ),
      Nil, //Classfiles don't support view bounds
      Nil  //Classfiles don't support context bounds
    )
  }

  def signatureToType(signature: Signature.JavaTypeSignature): Type = signature match {
    case Signature.ClassTypeSignature(
          packageSpecifier,
          Signature.SimpleClassTypeSignature(identifier, typeArguments),
          classTypeSignatureSuffix //TODO: Use suffix
        ) =>
      val packages   = packageSpecifier.toList.flatMap(_.toList)
      val packageRef = packageTermRef(packages)
      val idName     = Type.Name(identifier)
      val rawType    = packageRef.fold(idName: Type)(Type.Select(_, idName))

      typeArguments.fold(rawType) { arguments =>
        Type.Apply(
          rawType,
          arguments.toList.map {
            case Signature.TypeArgument.Wildcard => Type.Placeholder(Type.Bounds(None, None))

            case Signature.TypeArgument.Argument(Some(indicator), referenceTypeSignature) =>
              val tpe = signatureToType(referenceTypeSignature)

              val (lo, hi) = indicator match {
                case Signature.WildcardIndicator.Plus => (None, Some(tpe))
                case Signature.WildcardIndicator.Minus => (Some(tpe), None)
              }

              Type.Placeholder(Type.Bounds(lo, hi))
            case Signature.TypeArgument.Argument(None, referenceTypeSignature) =>
              signatureToType(referenceTypeSignature)
          }.toList
        )
      }
    case Signature.TypeVariableSignature(identifier) =>
      Type.Name(identifier)
    case Signature.ArrayTypeSignature(javaTypeSignature) =>
      t"scala.Array[${signatureToType(javaTypeSignature)}]"
    case Signature.BaseType.ByteTpe    => t"scala.Byte"
    case Signature.BaseType.CharTpe    => t"scala.Char"
    case Signature.BaseType.DoubleTpe  => t"scala.Double"
    case Signature.BaseType.FloatTpe   => t"scala.Float"
    case Signature.BaseType.IntTpe     => t"scala.Int"
    case Signature.BaseType.LongTpe    => t"scala.Long"
    case Signature.BaseType.ShortTpe   => t"scala.Short"
    case Signature.BaseType.BooleanTpe => t"scala.Boolean"
  }

  def classInfoToType(info: ClassInfo): Type = {
    val parts = info.name.split("/")
    if (parts.length > 1) {
      Type.Select(
        parts
          .slice(1, parts.length - 1)
          .foldLeft(Term.Name(parts.head): Term.Ref)((acc, str) => Term.Select(acc, Term.Name(str))),
        Type.Name(parts.last)
      )
    } else Type.Name(parts.head)
  }

  def classInfoWithSignatureToType(info: ClassInfo, signature: Signature.ClassTypeSignature): Type = {
    val parts          = info.name.split("/").toSeq
    val genericPackage = signature.packageSpecifier.toList.flatMap(_.toList)
    val genericParts   = genericPackage :+ signature.simpleClassTypeSignature.identifier

    if (parts == genericParts) {
      val typeName = Type.Name(signature.simpleClassTypeSignature.identifier)
      val rawType  = packageTermRef(genericPackage).fold(typeName: Type)(Type.Select(_, typeName))

      signature.simpleClassTypeSignature.typeArguments.fold(rawType: Type)(_ => signatureToType(signature))
    } else classInfoToType(info)
  }

  def signatureWithDescriptorToType(
      signature: Signature.JavaTypeSignature,
      descriptor: Descriptor.FieldType
  ): Type = (descriptor, signature) match {
    case (Descriptor.BaseType.ByteTpe, _)    => fieldDescriptorToType(descriptor)
    case (Descriptor.BaseType.CharTpe, _)    => fieldDescriptorToType(descriptor)
    case (Descriptor.BaseType.DoubleTpe, _)  => fieldDescriptorToType(descriptor)
    case (Descriptor.BaseType.FloatTpe, _)   => fieldDescriptorToType(descriptor)
    case (Descriptor.BaseType.IntTpe, _)     => fieldDescriptorToType(descriptor)
    case (Descriptor.BaseType.LongTpe, _)    => fieldDescriptorToType(descriptor)
    case (Descriptor.BaseType.ShortTpe, _)   => fieldDescriptorToType(descriptor)
    case (Descriptor.BaseType.BooleanTpe, _) => fieldDescriptorToType(descriptor)

    case (Descriptor.ArrayType(tpe), Signature.ArrayTypeSignature(javaTypeSignature)) =>
      t"scala.Array[${signatureWithDescriptorToType(javaTypeSignature, tpe)}]"

    case (_, Signature.TypeVariableSignature(identifier)) =>
      Type.Name(identifier) //TODO: Do we want to do more verification here?

    case (
          desc @ Descriptor.ObjectType(className),
          sig @ Signature.ClassTypeSignature(packageSpecifier, simpleClassTypeSignature, classTypeSignatureSuffix)
        ) =>
      val parts        = className.split("/")
      val packagePartD = parts.init.toSeq
      val packagePartS = packageSpecifier.toList.flatMap(_.toList)
      val classTypeD   = parts.last
      val classTypeS   = simpleClassTypeSignature.identifier

      if (packagePartD == packagePartS && classTypeD == classTypeS) signatureToType(sig)
      else fieldDescriptorToType(desc)
    case (d, _) => fieldDescriptorToType(d)
  }

  def typeSignatureResultWithDescriptorToType(
      signature: Signature.Result,
      descriptor: Descriptor.ReturnType
  ): Type = (signature, descriptor) match {
    case (_, Descriptor.VoidType) => t"scala.Unit"
    case (signature: Signature.JavaTypeSignature, descriptor: Descriptor.FieldType) =>
      signatureWithDescriptorToType(signature, descriptor)
    case (_, descriptor: Descriptor.FieldType) => fieldDescriptorToType(descriptor)
  }

  def dataToString(data: Either[_, _]): Term = data match {
    case Right(value) => Term.Block(List(Lit.String(formatData(value)), q"???"))
    case Left(value)  => Term.Block(List(Lit.String("Error: \n" + formatData(value)), q"???"))
  }

  def formatData(data: Any): String = {
    val lines = data match {
      case nel: NEL[_] => nel.toList
      case seq: Seq[_] => seq
      case _           => data.toString.split("\n").toVector
    }

    lines.mkString("\n" + "  " * 2 + "|", "\n" + "  " * 2 + "|", "")
  }

  def classObjToSyntax[FE, FA, ME, MA](
      classObj: ClassfileWithData[FE, FA, ME, MA],
      fieldDataHandler: Either[FE, FA] => Term = dataToString(_),
      methodDataHandler: Either[ME, MA] => Term = dataToString(_)
  ): Defn.Class = {

    val (tParams, genericSuperclass, genericSuperInterface) = classObj.attributes.collectFirst {
      case Signature(Signature.ClassSignature(typeParameters, superclassSignature, superinterfaceSignature)) =>
        val tParams = typeParameters.toSeq.flatMap(_.toList).map(typeParameterToType)

        (
          tParams,
          superclassSignature,
          superinterfaceSignature
        )
    }.unzip3

    val superClass = classObj.superClass.map(info =>
      genericSuperclass.fold(classInfoToType(info))(classInfoWithSignatureToType(info, _))
    )
    val interfaces = genericSuperInterface.fold(classObj.interfaces.map(classInfoToType)) { genericInterfaces =>
      classObj.interfaces.zip(genericInterfaces).map(t => classInfoWithSignatureToType(t._1, t._2))
    }

    val fields = classObj.fields.map { field =>
      val accessFlags = field.accessFlags
      val isFinal     = accessFlags.contains(AccessFlag.Final)

      val accessMods     = accessFlagsToMods(accessFlags)
      val annotationMods = annotationsFromAttributes(field.attributes)
      val mods           = accessMods.toList ++ annotationMods

      val pat = Pat.Var(Term.Name(field.name))

      val tpe = field.attributes
        .collectFirst {
          case Signature(sig: Signature.FieldSignature) =>
            sig match {
              case sig: Signature.ReferenceTypeSignature => signatureWithDescriptorToType(sig, field.descriptor)
            }
        }
        .getOrElse(fieldDescriptorToType(field.descriptor))

      val data = fieldDataHandler(field.data)

      if (isFinal) Defn.Val(mods, List(pat), Some(tpe), data)
      else Defn.Var(mods, List(pat), Some(tpe), Some(data))
    }

    val methods = classObj.methods.map { method =>
      val accessMods     = accessFlagsToMods(method.accessFlags)
      val annotationMods = annotationsFromAttributes(method.attributes)

      lazy val rawParamTypes = method.descriptor.paramTypes.map(fieldDescriptorToType)

      lazy val rawReturnType = method.descriptor.returnType match {
        case fieldType: Descriptor.FieldType => fieldDescriptorToType(fieldType)
        case Descriptor.VoidType             => Type.Select(Term.Name("scala"), Type.Name("Unit"))
      }

      val rawThrownTypes = method.attributes.collectFirst {
        case Exceptions(throwsTypes) => throwsTypes
      }

      val (tParams, genericInfo) = method.attributes.collectFirst {
        case Signature(Signature.MethodSignature(typeParameters, javaTypeSignatures, result, throwsSignature)) =>
          val typeParams = typeParameters.toList.flatMap(_.toList).map(typeParameterToType)

          (typeParams, (javaTypeSignatures, result, throwsSignature))
      }.unzip

      val (genericParamsTypes, genericResultType, genericThrownTypes) = genericInfo.unzip3

      val paramTypes = genericParamsTypes.fold(rawParamTypes: Seq[Type])(
        _.zip(method.descriptor.paramTypes).map(t => signatureWithDescriptorToType(t._1, t._2))
      )

      val returnType =
        genericResultType.fold(rawReturnType)(typeSignatureResultWithDescriptorToType(_, method.descriptor.returnType))

      val throwsTypes = (rawThrownTypes, genericThrownTypes) match {
        case (Some(rawThrownTypes), Some(genericThrownTypes)) =>
          rawThrownTypes.zip(genericThrownTypes).map {
            case (_, Signature.ThrowsSignature.ThrowTypeVar(Signature.TypeVariableSignature(tpe))) => Type.Name(tpe)
            case (classInfo, Signature.ThrowsSignature.ThrowClass(signature)) =>
              classInfoWithSignatureToType(classInfo, signature)
          }

        case (Some(rawThrownTypes), None) => rawThrownTypes.map(classInfoToType)
        case _                            => Seq.empty
      }

      val throwsMods = throwsTypes.map(tpe => mod"@throws[$tpe]")

      val paramAnnotations =
        paramterAnnotationsFromAttributes(method.attributes).getOrElse(Vector.fill(paramTypes.length)(Vector.empty))
      val params = paramTypes.zip(Vector.fill(paramTypes.length)(None)).zip(paramAnnotations).zipWithIndex.map {
        case (((tpe, paramInfo), annotations), i) =>
          val mods = annotations //++ ???

          val name = paramInfo.map(_ => ???).getOrElse(s"x$i")

          Term.Param(
            mods.toList,
            Term.Name(name),
            Some(tpe),
            None
          )
      }

      Defn.Def(
        (accessMods ++ annotationMods ++ throwsMods).toList,
        Term.Name(method.name),
        tParams.toList.flatten,
        List(params.toList),
        Some(returnType),
        methodDataHandler(method.data)
      )
    }

    val accessMods     = accessFlagsToMods(classObj.accessFlags)
    val annotationMods = annotationsFromAttributes(classObj.attributes)

    Defn.Class(
      accessMods.toList ++ annotationMods,
      Type.Name(classObj.thisClass.name),
      tParams.toList.flatten,
      Ctor.Primary(Nil, Name.Anonymous(), Nil), //We treat the constructor as a normal method at this point
      Template(
        Nil,
        (superClass.toList ++ interfaces).map(t => Init(t, Name.Anonymous(), Nil)),
        Self(Name.Anonymous(), None),
        (fields ++ methods).toList
      )
    )
  }

}
