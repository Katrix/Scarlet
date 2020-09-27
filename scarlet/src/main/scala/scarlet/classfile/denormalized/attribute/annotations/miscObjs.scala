package scarlet.classfile.denormalized.attribute.annotations
import scarlet.classfile.denormalized.ConstantPoolEntry.ClassInfo
import scarlet.classfile.denormalized.Descriptor

case class Annotation(fieldType: Descriptor.FieldType, elementValuePairs: Vector[ElementValuePair])
case class ElementValuePair(elementName: String, value: ElementValue)
sealed abstract class ElementValue(val tag: Char)
object ElementValue {
  sealed trait ConstElementValue[A] extends ElementValue {
    def const: A
  }

  case class ByteValue(const: Byte)                                       extends ElementValue('B') with ConstElementValue[Byte]
  case class CharValue(const: Char)                                       extends ElementValue('C') with ConstElementValue[Char]
  case class DoubleValue(const: Double)                                   extends ElementValue('D') with ConstElementValue[Double]
  case class FloatValue(const: Float)                                     extends ElementValue('F') with ConstElementValue[Float]
  case class IntValue(const: Int)                                         extends ElementValue('I') with ConstElementValue[Int]
  case class LongValue(const: Long)                                       extends ElementValue('J') with ConstElementValue[Long]
  case class ShortValue(const: Short)                                     extends ElementValue('S') with ConstElementValue[Short]
  case class BooleanValue(const: Boolean)                                 extends ElementValue('Z') with ConstElementValue[Boolean]
  case class StringValue(const: String)                                   extends ElementValue('s') with ConstElementValue[String]
  case class EnumValue(typeName: Descriptor.FieldType, constName: String) extends ElementValue('e')
  case class ClassValue(classInfo: ClassInfo)                             extends ElementValue('c')
  case class AnnotationValue(annotation: Annotation)                      extends ElementValue('@')
  case class ArrayValue(values: Vector[ElementValue])                     extends ElementValue('[')
}

case class TypeAnnotation(
    targetInfoAndType: TargetType,
    targetPath: TypePath,
    fieldType: Descriptor.FieldType,
    elementValuePairs: Vector[ElementValuePair]
)

sealed abstract class TargetType(val byteValue: Int)
object TargetType {
  import TargetInfo._
  case class ClassType(targetInfo: TypeParameterTarget)              extends TargetType(0x00)
  case class MethodType(targetInfo: TypeParameterTarget)             extends TargetType(0x01)
  case class ExtendsType(targetInfo: SupertypeTarget)                extends TargetType(0x10)
  case class ClassBoundsType(targetInfo: TypeParameterBoundTarget)   extends TargetType(0x11)
  case class MethodBoundsType(targetInfo: TypeParameterBoundTarget)  extends TargetType(0x12)
  case object FieldType                                              extends TargetType(0x13)
  case object ReturnType                                             extends TargetType(0x14)
  case object ReceiverTypeType                                       extends TargetType(0x15)
  case class FormalParameterType(targetInfo: FormalParameterTarget)  extends TargetType(0x16)
  case class ThrowsType(targetInfo: ThrowsTarget)                    extends TargetType(0x17)
  case class LocalVarType(targetInfo: LocalVarTarget)                extends TargetType(0x40)
  case class ResourceVarType(targetInfo: LocalVarTarget)             extends TargetType(0x41)
  case class ExceptionParamType(targetInfo: CatchTarget)             extends TargetType(0x42)
  case class InstanceOfType(targetInfo: OffsetTarget)                extends TargetType(0x43)
  case class NewType(targetInfo: OffsetTarget)                       extends TargetType(0x44)
  case class MethodRefNewType(targetInfo: OffsetTarget)              extends TargetType(0x45)
  case class MethodRefType(targetInfo: OffsetTarget)                 extends TargetType(0x46)
  case class CastType(targetInfo: TypeArgumentTarget)                extends TargetType(0x47)
  case class NewGenericType(targetInfo: TypeArgumentTarget)          extends TargetType(0x48)
  case class MethodGenericType(targetInfo: TypeArgumentTarget)       extends TargetType(0x49)
  case class MethodRefNewGenericType(targetInfo: TypeArgumentTarget) extends TargetType(0x4A)
  case class MethodRefGenericType(targetInfo: TypeArgumentTarget)    extends TargetType(0x4B)

  object TargetInfo {
    case class TypeParameterTarget(typeParamIdx: Int)
    case class SupertypeTarget(supertypeIdx: Int)
    case class TypeParameterBoundTarget(typeParamIdx: Int, boundIdx: Int)
    type EmptyTarget = EmptyTarget.type
    case object EmptyTarget //Do I really need this?
    case class FormalParameterTarget(formalParamIdx: Int)
    case class ThrowsTarget(throwsTypeIdx: Int)
    case class LocalVar(startPc: Int, length: Int, index: Int)
    case class LocalVarTarget(localVars: Vector[LocalVar])
    case class CatchTarget(exceptionTableIdx: Int)
    case class OffsetTarget(offset: Int)
    case class TypeArgumentTarget(offset: Int, typeArgumentIdx: Int)
  }
}

case class TypePathElem(typePathKind: Int, typeArgumentIdx: Int)
case class TypePath(paths: Vector[TypePathElem])
