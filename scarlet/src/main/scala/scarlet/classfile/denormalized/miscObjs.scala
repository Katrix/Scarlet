package scarlet.classfile.denormalized
import scarlet.classfile.denormalized.attribute.{Attribute, Code}

/**
  * A field of a classfile.
  */
case class FieldInfo(
    accessFlags: Set[AccessFlag],
    name: String,
    descriptor: Descriptor.FieldType,
    attributes: Vector[Attribute]
)

/**
  * A method of a classfile.
  */
case class MethodInfo(
    accessFlags: Set[AccessFlag],
    name: String,
    descriptor: Descriptor.MethodDescriptor,
    attributes: Vector[Attribute]
)

/**
  * Represents the owner of an attribute. Used so that an attribute can
  * inspect it's parent when expanding.
  */
sealed trait AttributeOwner {
  def typeStr: String
}
object AttributeOwner {
  case class ClassOwner(classfile: Classfile) extends AttributeOwner {
    override def typeStr: String = "class"
  }
  case class FieldOwner(field: FieldInfo) extends AttributeOwner {
    override def typeStr: String = "field"
  }
  case class MethodOwner(method: MethodInfo) extends AttributeOwner {
    override def typeStr: String = "method"
  }
  case class CodeOwner(code: Code) extends AttributeOwner {
    override def typeStr: String = "code"
  }
}
