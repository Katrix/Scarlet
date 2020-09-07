package scarlet.ir

import scarlet.classfile.denormalized.ConstantPoolEntry.ClassInfo
import scarlet.classfile.denormalized.attribute.Attribute
import scarlet.classfile.denormalized.{AccessFlag, ConstantPool}
import scarlet.classfile.shared.Version

import cats.syntax.all._

case class ClassfileWithData[FieldE, FieldA, MethodE, MethodA](
    version: Version,
    constantPool: ConstantPool,
    accessFlags: Set[AccessFlag],
    thisClass: ClassInfo,
    superClass: Option[ClassInfo],
    interfaces: Vector[ClassInfo],
    fields: Vector[FieldWithData[FieldE, FieldA]],
    methods: Vector[MethodWithData[MethodE, MethodA]],
    attributes: Vector[Attribute]
) {

  def fullmapMethod[E2, A2](
      f: Either[MethodE, MethodA] => Either[E2, A2]
  ): ClassfileWithData[FieldE, FieldA, E2, A2] =
    copy(methods = methods.map(_.map(f)))

  def fullmapMethodWithMethod[E2, A2](
      f: (MethodWithData[MethodE, MethodA], Either[MethodE, MethodA]) => Either[E2, A2]
  ): ClassfileWithData[FieldE, FieldA, E2, A2] =
    copy(methods = methods.map(_.mapWithMethod(f)))

  def leftmapMethod[E2](f: MethodE => E2): ClassfileWithData[FieldE, FieldA, E2, MethodA]  = fullmapMethod(_.leftMap(f))
  def rightmapMethod[A2](f: MethodA => A2): ClassfileWithData[FieldE, FieldA, MethodE, A2] = fullmapMethod(_.map(f))

  def leftmapMethodWithMethod[E2](
      f: (MethodWithData[MethodE, MethodA], MethodE) => E2
  ): ClassfileWithData[FieldE, FieldA, E2, MethodA] = fullmapMethodWithMethod((m, e) => e.leftMap(f(m, _)))
  def rightmapMethodWithMethod[A2](
      f: (MethodWithData[MethodE, MethodA], MethodA) => A2
  ): ClassfileWithData[FieldE, FieldA, MethodE, A2] = fullmapMethodWithMethod((m, e) => e.map(f(m, _)))

  def fullmapField[E2, A2](f: Either[FieldE, FieldA] => Either[E2, A2]): ClassfileWithData[E2, A2, MethodE, MethodA] =
    copy(fields = fields.map(_.map(f)))

  def leftmapField[E2](f: FieldE => E2): ClassfileWithData[E2, FieldA, MethodE, MethodA]  = fullmapField(_.leftMap(f))
  def rightmapField[A2](f: FieldA => A2): ClassfileWithData[FieldE, A2, MethodE, MethodA] = fullmapField(_.map(f))
}
