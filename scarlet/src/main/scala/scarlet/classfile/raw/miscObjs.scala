package scarlet.classfile.raw

import cats.data.ValidatedNel
import scarlet.classfile.denormalized
import scodec.Err
import scodec.bits.ByteVector
import cats.syntax.all._
import cats.instances.vector._

case class AccessFlags(flags: Int)

case class ThisClass(constIdx: Int)

case class SuperClass(constIdx: Int)

case class InterfaceEntry(constIdx: Int)
case class Interfaces(interfaces: Vector[InterfaceEntry])

case class FieldInfo(accessFlags: AccessFlags, nameIdx: Int, descriptorIdx: Int, attributes: Attributes)
case class Fields(fields: Vector[FieldInfo])

case class MethodInfo(accessFlags: AccessFlags, nameIdx: Int, descriptorIdx: Int, attributes: Attributes)
case class Methods(methods: Vector[MethodInfo])

case class AttributeInfo(nameIdx: Int, info: ByteVector)
case class Attributes(attributes: Vector[AttributeInfo]) {

  import denormalized.{ConstantPoolEntry => NormConstPoolEntry}

  /**
    * Pairs each attribute up with their name. At this point all attributes are
    * mapped to [[denormalized.attribute.Unknown]]. Expanding them comes later.
    */
  def denormalize(
      constPool: denormalized.ConstantPool
  ): ValidatedNel[Err, Vector[denormalized.attribute.Unknown]] =
    attributes.traverse {
      case AttributeInfo(nameIdx, info) =>
        constPool
          .get[NormConstPoolEntry.Utf8Info](nameIdx)
          .map(utf8 => denormalized.attribute.Unknown(utf8.string, info))
    }
}
