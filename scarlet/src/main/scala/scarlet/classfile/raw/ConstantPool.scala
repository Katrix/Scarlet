package scarlet.classfile.raw

import java.io.{ByteArrayInputStream, DataInputStream, PrintWriter, StringWriter}

import cats.instances.vector._
import cats.syntax.all._
import scarlet.classfile.denormalized.Descriptor
import scarlet.classfile.{denormalized, raw}
import scarlet.classfile.shared.Version
import scodec.Err
import scodec.bits.ByteVector
import shapeless.{TypeCase, Typeable}
import scala.util.Try

import cats.data.Validated.Valid
import cats.data.{NonEmptyList, ValidatedNel}

/**
  * A raw constant pool, as read from the classfile.
  */
case class ConstantPool(entries: Vector[ConstantPoolEntry]) {

  /**
    * Denormalizes this constant pool.
    */
  def denormalize(version: Version): ValidatedNel[Err, denormalized.ConstantPool] = {
    import denormalized.{ConstantPoolEntry => NewE}
    import raw.{ConstantPoolEntry => RawE}

    //In other places denormalization generally just means fetching references
    // to the constant pool. It still means that here, but here we need to pay
    // more attention to getting everything done in the correct order, while
    // at the same time preserving as many errors as possible. As such, things
    // are a bit more verbose in here, than if we had used a for comprehension.

    def createInfoMap[A: Typeable, B <: NewE](parse: A => ValidatedNel[Err, B]) = {
      val AType = TypeCase[A]
      entries.zipWithIndex
        .collect {
          case (AType(a), idx) =>
            parse(a).tupleLeft(idx + 1)
        }
        .sequence
        .map(_.toMap)
    }

    def getFromMap[A, B](idx: Int, map: Map[Int, A])(implicit tpe: Typeable[A], useTpe: Typeable[B]) =
      map.get(idx).toValidNel(Err(s"Could not find ${tpe.describe} for ${useTpe.describe}"))

    val validatedUtf8Infos = createInfoMap[RawE.Utf8Info, NewE.Utf8Info] {
      case RawE.Utf8Info(data) =>
        val lengthData = ByteVector.fromShort(data.length.toShort)
        Try(new DataInputStream(new ByteArrayInputStream((lengthData ++ data).toArray)).readUTF()).toEither
          .map(s => NewE.Utf8Info(s))
          .leftMap { e =>
            val sw = new StringWriter()
            e.printStackTrace(new PrintWriter(sw))
            Err(sw.toString)
          }
          .toValidatedNel
    }

    val validatedClassInfos = validatedUtf8Infos.andThen { utf8Infos =>
      createInfoMap[RawE.ClassInfo, NewE.ClassInfo] {
        case RawE.ClassInfo(nameIdx) =>
          getFromMap[NewE.Utf8Info, NewE.ClassInfo](nameIdx, utf8Infos).map(name => NewE.ClassInfo(name.string))
      }
    }

    val validatedNameAndTypeInfos = validatedUtf8Infos.andThen { utf8Infos =>
      createInfoMap[RawE.NameAndTypeInfo, NewE.NameAndTypeInfo] {
        case RawE.NameAndTypeInfo(nameIdx, descriptorIdx) =>
          (
            getFromMap[NewE.Utf8Info, NewE.NameAndTypeInfo](nameIdx, utf8Infos),
            getFromMap[NewE.Utf8Info, NewE.NameAndTypeInfo](descriptorIdx, utf8Infos)
              .andThen(d => Descriptor.parseStr(d.string))
          ).mapN((name, descriptor) => NewE.NameAndTypeInfo(name.string, descriptor))
      }
    }

    val validatedClassNameAndTypeInfos = validatedClassInfos.product(validatedNameAndTypeInfos)

    val validatedFieldRefInfos = validatedClassNameAndTypeInfos.andThen {
      case (classInfos, nameAndTypeInfos) =>
        createInfoMap[RawE.FieldRefInfo, NewE.FieldRefInfo] {
          case RawE.FieldRefInfo(classIdx, nameAndTypeIdx) =>
            (
              getFromMap[NewE.ClassInfo, NewE.FieldRefInfo](classIdx, classInfos),
              getFromMap[NewE.NameAndTypeInfo, NewE.FieldRefInfo](nameAndTypeIdx, nameAndTypeInfos)
            ).mapN(NewE.FieldRefInfo)
        }
    }

    val validatedMethodRefInfos = validatedClassNameAndTypeInfos.andThen {
      case (classInfos, nameAndTypeInfos) =>
        createInfoMap[RawE.MethodRefInfo, NewE.MethodRefInfo] {
          case RawE.MethodRefInfo(classIdx, nameAndTypeIdx) =>
            (
              getFromMap[NewE.ClassInfo, NewE.MethodRefInfo](classIdx, classInfos),
              getFromMap[NewE.NameAndTypeInfo, NewE.MethodRefInfo](nameAndTypeIdx, nameAndTypeInfos)
            ).mapN(NewE.MethodRefInfo)
        }
    }

    val validatedInterfaceMethodRefInfos = validatedClassNameAndTypeInfos.andThen {
      case (classInfos, nameAndTypeInfos) =>
        createInfoMap[RawE.InterfaceMethodRefInfo, NewE.InterfaceMethodRefInfo] {
          case RawE.InterfaceMethodRefInfo(classIdx, nameAndTypeIdx) =>
            (
              getFromMap[NewE.ClassInfo, NewE.InterfaceMethodRefInfo](classIdx, classInfos),
              getFromMap[NewE.NameAndTypeInfo, NewE.InterfaceMethodRefInfo](
                nameAndTypeIdx,
                nameAndTypeInfos
              )
            ).mapN(NewE.InterfaceMethodRefInfo)
        }
    }

    val validatedStringInfos = validatedUtf8Infos.andThen { utf8Infos =>
      createInfoMap[RawE.StringInfo, NewE.StringInfo] {
        case RawE.StringInfo(stringIdx) =>
          getFromMap[NewE.Utf8Info, NewE.StringInfo](stringIdx, utf8Infos).map(utf8 => NewE.StringInfo(utf8.string))
      }
    }

    val validatedIntegerInfos = createInfoMap[RawE.IntegerInfo, NewE.IntegerInfo] {
      case RawE.IntegerInfo(bytes) =>
        Valid(NewE.IntegerInfo(bytes.toInt))
    }

    val validatedFloatInfos = createInfoMap[RawE.FloatInfo, NewE.FloatInfo] {
      case RawE.FloatInfo(bytes) =>
        Valid(NewE.FloatInfo(java.lang.Float.intBitsToFloat(bytes.toInt)))
    }

    val validatedLongInfos = createInfoMap[RawE.LongInfo, NewE.LongInfo] {
      case RawE.LongInfo(high, low) =>
        Valid(NewE.LongInfo(low << 32 + high))
    }

    val validatedDoubleInfos = createInfoMap[RawE.DoubleInfo, NewE.DoubleInfo] {
      case RawE.DoubleInfo(high, low) =>
        Valid(NewE.DoubleInfo(java.lang.Double.longBitsToDouble(low << 32 + high)))
    }

    lazy val validatedMethodInterfaceMethodRefInfos = validatedMethodRefInfos.product(validatedInterfaceMethodRefInfos)

    val validatedMethodHandleInfos = createInfoMap[RawE.MethodHandleInfo, NewE.MethodHandleInfo] {
      case RawE.MethodHandleInfo(referenceKind, referenceIdx) =>
        NewE.RefKind.fromInt(referenceKind).toValidNel(Err("Invalid reference kind on MethodHandle")).andThen {
          case kind: NewE.RefKind.FieldRefKind =>
            validatedFieldRefInfos.andThen { fieldRefInfos =>
              getFromMap[NewE.FieldRefInfo, NewE.FieldMethodHandleInfo](referenceIdx, fieldRefInfos)
                .map(refInfo => NewE.FieldMethodHandleInfo(kind, refInfo))
            }
          case kind: NewE.RefKind.MethodRefKind =>
            validatedMethodRefInfos.andThen { methodRefInfos =>
              getFromMap[NewE.MethodRefInfo, NewE.MethodMethodHandleInfo](referenceIdx, methodRefInfos)
                .map(refInfo => NewE.MethodMethodHandleInfo(kind, refInfo))
            }
          case kind: NewE.RefKind.InterfaceMethodOrMethodRefKind =>
            if (version.minor < 52) {
              validatedMethodRefInfos.andThen { methodRefInfos =>
                getFromMap[NewE.MethodRefInfo, NewE.InterfaceMethodOrMethodMethodHandleInfo](
                  referenceIdx,
                  methodRefInfos
                ).map(refInfo => NewE.InterfaceMethodOrMethodMethodHandleInfo(kind, Left(refInfo)))
              }
            } else {
              validatedMethodInterfaceMethodRefInfos.andThen {
                case (methodRefInfos, interfaceMethodRefInfos) =>
                  getFromMap[NewE.MethodRefInfo, NewE.InterfaceMethodOrMethodMethodHandleInfo](
                    referenceIdx,
                    methodRefInfos
                  ).map(refInfo => NewE.InterfaceMethodOrMethodMethodHandleInfo(kind, Left(refInfo))).orElse {
                    getFromMap[NewE.InterfaceMethodRefInfo, NewE.InterfaceMethodOrMethodMethodHandleInfo](
                      referenceIdx,
                      interfaceMethodRefInfos
                    ).map(refInfo => NewE.InterfaceMethodOrMethodMethodHandleInfo(kind, Right(refInfo)))
                  }
              }
            }
          case kind: NewE.RefKind.InterfaceMethodRefKind =>
            validatedInterfaceMethodRefInfos.andThen { interfaceMethodRefInfos =>
              getFromMap[NewE.InterfaceMethodRefInfo, NewE.InterfaceMethodMethodHandleInfo](
                referenceIdx,
                interfaceMethodRefInfos
              ).map(refInfo => NewE.InterfaceMethodMethodHandleInfo(kind, refInfo))
            }
        }
    }

    val validatedMethodTypeInfos = validatedUtf8Infos.andThen { utf8Infos =>
      createInfoMap[RawE.MethodTypeInfo, NewE.MethodTypeInfo] {
        case RawE.MethodTypeInfo(descriptorIdx) =>
          getFromMap[NewE.Utf8Info, NewE.MethodTypeInfo](descriptorIdx, utf8Infos)
            .andThen(d => Descriptor.parseMethodStr(d.string))
            .map(NewE.MethodTypeInfo)
      }
    }

    val validatedInvokeDynamicInfos = validatedNameAndTypeInfos.andThen { nameAndTypeInfos =>
      createInfoMap[RawE.InvokeDynamicInfo, NewE.InvokeDynamicInfo] {
        case RawE.InvokeDynamicInfo(bootstrapMethodAttrIdx, nameAndTypeIdx) =>
          getFromMap[NewE.NameAndTypeInfo, NewE.InvokeDynamicInfo](nameAndTypeIdx, nameAndTypeInfos)
            .map(nameAndType => NewE.InvokeDynamicInfo(bootstrapMethodAttrIdx, nameAndType))
      }
    }

    val validatedInfos = (
      validatedUtf8Infos,
      validatedClassInfos,
      validatedNameAndTypeInfos,
      validatedFieldRefInfos,
      validatedMethodRefInfos,
      validatedInterfaceMethodRefInfos,
      validatedStringInfos,
      validatedIntegerInfos,
      validatedFloatInfos,
      validatedLongInfos,
      validatedDoubleInfos,
      validatedMethodHandleInfos,
      validatedMethodTypeInfos,
      validatedInvokeDynamicInfos
    ).mapN(_ ++ _ ++ _ ++ _ ++ _ ++ _ ++ _ ++ _ ++ _ ++ _ ++ _ ++ _ ++ _ ++ _)

    validatedInfos
      .leftMap(xs => NonEmptyList.fromListUnsafe(xs.toList.distinct))
      .map(infos => denormalized.ConstantPool(infos.toVector.sortBy(_._1).map(_._2)))
  }
}
