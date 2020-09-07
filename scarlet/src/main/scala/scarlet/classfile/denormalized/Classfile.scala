package scarlet.classfile.denormalized

import cats.data.ValidatedNel
import scarlet.classfile.denormalized.ConstantPoolEntry._
import scarlet.classfile.shared.Version
import scodec._
import cats.syntax.all._
import cats.instances.vector._
import scarlet.classfile.denormalized.attribute.{Attribute, NamedAttributeCompanion}

/**
  * A denormalized classfile.
  */
case class Classfile(
    version: Version,
    constantPool: ConstantPool,
    accessFlags: Set[AccessFlag],
    thisClass: ClassInfo,
    superClass: Option[ClassInfo],
    interfaces: Vector[ClassInfo],
    fields: Vector[FieldInfo],
    methods: Vector[MethodInfo],
    attributes: Vector[Attribute]
) {

  /**
    * Expand the unknown attributes of this classfile. Applies to both root
    * attributes, field attributes and method attributes.
    * @param companions A sequence of companions to consider when expanding
    *                   attributes. Only attributes in this sequence will be
    *                   considered when expanding.
    */
  def expandAttributes(companions: Seq[NamedAttributeCompanion[_ <: Attribute]]): ValidatedNel[Err, Classfile] = {
    val companionsSeqTuple: Seq[(String, NamedAttributeCompanion[_ <: Attribute])] = companions.map(c => c.name -> c)
    val companionsMap                                                              = companionsSeqTuple.toMap

    (
      attributes.traverse(_.expandUnknown(AttributeOwner.ClassOwner(this), constantPool, companionsMap)),
      fields.traverse { f =>
        f.attributes
          .traverse(_.expandUnknown(AttributeOwner.FieldOwner(f), constantPool, companionsMap))
          .map(newAtt => f.copy(attributes = newAtt))
      },
      methods.traverse { m =>
        m.attributes
          .traverse(_.expandUnknown(AttributeOwner.MethodOwner(m), constantPool, companionsMap))
          .map(newAtt => m.copy(attributes = newAtt))
      }
    ).mapN { (newClassAttributes, newFields, newMethods) =>
      copy(
        attributes = newClassAttributes,
        fields = newFields,
        methods = newMethods
      )
    }
  }
}
