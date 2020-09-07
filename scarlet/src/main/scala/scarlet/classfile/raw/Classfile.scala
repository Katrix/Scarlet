package scarlet.classfile.raw

import cats.data.ValidatedNel
import cats.instances.option._
import cats.instances.vector._
import cats.syntax.all._
import scarlet.classfile.denormalized.Descriptor
import scarlet.classfile.{denormalized, shared}
import scodec.Err

/**
  * A raw classfile as read from the file passed in without any processing.
  */
case class Classfile(
    version: shared.Version,
    constantPool: ConstantPool,
    accessFlags: AccessFlags,
    thisClass: ThisClass,
    superClass: SuperClass,
    interfaces: Interfaces,
    fields: Fields,
    methods: Methods,
    attributes: Attributes
) {
  import denormalized.{ConstantPoolEntry => DeNormConstPoolEntry}


  /**
    * Denormalize a field of this classfile.
    */
  private def denormalizeField(
      field: FieldInfo,
      constPool: denormalized.ConstantPool
  ): ValidatedNel[Err, denormalized.FieldInfo] =
    (
      constPool.get[DeNormConstPoolEntry.Utf8Info](field.nameIdx),
      constPool
        .get[DeNormConstPoolEntry.Utf8Info](field.descriptorIdx)
        .andThen(d => Descriptor.parseFieldStr(d.string)),
      field.attributes.denormalize(constPool)
    ).mapN { (name, descriptor, newAttributes) =>
      denormalized.FieldInfo(
        accessFlags = denormalized.AccessFlag.fromInt(field.accessFlags.flags),
        name = name.string,
        descriptor = descriptor,
        attributes = newAttributes
      )
    }

  /**
    * Denormalize a method of this classfile.
    */
  private def denormalizeMethod(
      method: MethodInfo,
      constPool: denormalized.ConstantPool
  ): ValidatedNel[Err, denormalized.MethodInfo] =
    (
      constPool.get[DeNormConstPoolEntry.Utf8Info](method.nameIdx),
      constPool
        .get[DeNormConstPoolEntry.Utf8Info](method.descriptorIdx)
        .andThen(d => Descriptor.parseMethodStr(d.string)),
      method.attributes.denormalize(constPool)
    ).mapN { (name, descriptor, newAttributes) =>
      denormalized.MethodInfo(
        accessFlags = denormalized.AccessFlag.fromInt(method.accessFlags.flags),
        name = name.string,
        descriptor = descriptor,
        attributes = newAttributes
      )
    }

  /**
    * Denormalize this classfile. This for the most part means just resolving
    * references to the constant pool, and parsing descriptors. Attributes
    * only resolve their name. Expanding them is left for another step.
    */
  def denormalize: ValidatedNel[Err, denormalized.Classfile] = {
    constantPool.denormalize(version).andThen { newConstPool =>
      (
        newConstPool.get[DeNormConstPoolEntry.ClassInfo](thisClass.constIdx), {
          val optInfo =
            if (superClass.constIdx == 0) None
            else Some(newConstPool.get[DeNormConstPoolEntry.ClassInfo](superClass.constIdx))
          optInfo.sequence
        },
        interfaces.interfaces.traverse { interface =>
          newConstPool.get[DeNormConstPoolEntry.ClassInfo](interface.constIdx)
        },
        fields.fields.traverse(denormalizeField(_, newConstPool)),
        methods.methods.traverse(denormalizeMethod(_, newConstPool)),
        attributes.denormalize(newConstPool)
      ).mapN { (thisClassInfo, superClassInfo, interfacesClassInfo, fieldsInfo, methodsInfo, newAttributes) =>
        denormalized.Classfile(
          version = version,
          constantPool = newConstPool,
          accessFlags = denormalized.AccessFlag.fromInt(accessFlags.flags),
          thisClass = thisClassInfo,
          superClass = superClassInfo,
          interfaces = interfacesClassInfo,
          fields = fieldsInfo,
          methods = methodsInfo,
          attributes = newAttributes
        )
      }
    }
  }
}
