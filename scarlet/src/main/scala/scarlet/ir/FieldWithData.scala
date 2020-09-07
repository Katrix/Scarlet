package scarlet.ir

import scarlet.classfile.denormalized.{AccessFlag, Descriptor}
import scarlet.classfile.denormalized.attribute.Attribute

case class FieldWithData[E, A](
    accessFlags: Set[AccessFlag],
    name: String,
    descriptor: Descriptor.FieldType,
    attributes: Vector[Attribute],
    data: Either[E, A]
) {

  def map[E2, A2](f: Either[E, A] => Either[E2, A2]): FieldWithData[E2, A2] = copy(data = f(data))
}
