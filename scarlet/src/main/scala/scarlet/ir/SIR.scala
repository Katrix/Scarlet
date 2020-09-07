package scarlet.ir

import scarlet.classfile.denormalized.ConstantPoolEntry.{ClassInfo, FieldRefInfo}
import scarlet.classfile.denormalized.Descriptor
import scarlet.classfile.denormalized.opcodes.OPCode
import scarlet.ir.SIR.Expr

/**
  * A stackless IR to help with not having to deal with the stack further into
  * the decompiler.
  *
  * Based on this paper: http://people.irisa.fr/David.Pichardie/papers/aplas10.pdf
  */
sealed trait SIR {

  /**
    * Substitute the target with a new expression, for all the expressions this instruction stores.
    */
  def substituteExpr[B](target: Expr[B], newExpr: Expr[B]): SIR = this
}
object SIR {

  /**
    * Unlike the expressions in the referenced paper, we do quite a bit of
    * type checking at this point, to catch stupid errors early.
    */
  sealed trait Type {
    type A

    /**
      * Describes this type.
      */
    def describe: String

    /**
      * Checks if this type is a supertype of the passed in type.
      */
    def isSupertypeOf(tpe: Type): Boolean = tpe == this || tpe == Type.Unknown
  }
  object Type {
    type Aux[A0] = Type { type A = A0 }
    sealed abstract class BaseType[A0](val describe: String) extends Type {
      type A = A0
    }

    case object Boolean extends BaseType[scala.Boolean]("Boolean")
    case object Byte    extends BaseType[scala.Byte]("Byte")
    case object Short   extends BaseType[scala.Short]("Short")
    case object Int     extends BaseType[scala.Int]("Int")
    case object Long    extends BaseType[scala.Long]("Long")
    case object Float   extends BaseType[scala.Float]("Float")
    case object Double  extends BaseType[scala.Double]("Double")
    case object Char    extends BaseType[scala.Char]("Char")
    case object Void    extends BaseType[scala.Nothing]("Void")

    case object String                   extends BaseType[Predef.String]("String")
    case object Class                    extends BaseType[Predef.String]("Class")
    case class Ref(classInfo: ClassInfo) extends BaseType[scala.AnyRef](classInfo.name)

    case class Array[A](tpe: Type.Aux[A]) extends BaseType[scala.Array[A]](s"Array[${tpe.describe}]") {
      override def isSupertypeOf(tpe: Type): Boolean = tpe match {
        case Array(otherTpe) => this.tpe.isSupertypeOf(otherTpe)
        case _               => super.isSupertypeOf(tpe)
      }
    }

    /** Common type for all arrays */
    case object AnyArray extends BaseType[scala.Array[_]]("Array[_]") {
      override def isSupertypeOf(tpe: Type): Boolean = tpe.isInstanceOf[Array[_]] || tpe == this || tpe == Type.Unknown
    }

    /** Common type for all reference types */
    case object AnyRef extends BaseType[scala.AnyRef]("AnyRef") {
      override def isSupertypeOf(tpe: Type): Boolean = tpe match {
        case String      => true
        case Class       => true
        case Ref(_)      => true
        case _: Array[_] => true
        case AnyArray    => true
        case AnyRef      => true
        case Unknown     => true
        case _           => false
      }
    }

    /** Top type for all */
    case object Any extends BaseType[scala.Any]("Any") {
      override def isSupertypeOf(tpe: Type): Boolean = true
    }

    /** The bottom type for all */
    case object Unknown extends BaseType[scala.Nothing]("Unknown") {
      override def isSupertypeOf(tpe: Type): Boolean = tpe == Unknown
    }

    /** Represents all category 1 types. Anything that is not a category 2 type */
    case object Category1 extends BaseType[scala.AnyVal]("Category 1 type") {
      override def isSupertypeOf(tpe: Type): Boolean = tpe match {
        case Long   => false
        case Double => false
        case _      => true
      }
    }

    /** Represents "big" types that takes two places on the stack */
    case object Category2 extends BaseType[scala.AnyVal]("Category 2 type") {
      override def isSupertypeOf(tpe: Type): Boolean = tpe match {
        case Long    => true
        case Double  => true
        case Unknown => true
        case _       => false
      }
    }

    def fromDescriptor(descriptor: Descriptor): Type.Aux[_] = {
      val retTpe = descriptor match {
        case descriptor: Descriptor.FieldType        => descriptor
        case descriptor: Descriptor.MethodDescriptor => descriptor.returnType
      }

      retTpe match {
        case Descriptor.BaseType.ByteTpe    => Type.Byte
        case Descriptor.BaseType.CharTpe    => Type.Char
        case Descriptor.BaseType.DoubleTpe  => Type.Double
        case Descriptor.BaseType.FloatTpe   => Type.Float
        case Descriptor.BaseType.IntTpe     => Type.Int
        case Descriptor.BaseType.LongTpe    => Type.Long
        case Descriptor.BaseType.ShortTpe   => Type.Short
        case Descriptor.BaseType.BooleanTpe => Type.Boolean
        case tpe: Descriptor.ObjectType =>
          Type.Ref(ClassInfo(tpe.className)) //TODO: Ensure formatting is correct for ClassInfo
        case tpe: Descriptor.ArrayType => Type.Array(fromDescriptor(tpe.tpe))
        case Descriptor.VoidType       => Type.Void
      }

    }
  }

  sealed trait Expr[A] {
    def tpe: Type.Aux[A]

    /**
      * Print this expression in a somewhat readable format
      */
    def toSyntax: String

    /**
      * Substitutes this expression, or it's children with a new expression.
      * @param target The expression to test against.
      * @param newExpr The new expression to use if this was equal to the target.
      */
    def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[A] =
      if (target == this) newExpr.asInstanceOf[Expr[A]] else this
  }
  object Expr {
    case class UninitializedRef(atAddress: Long, classInfo: ClassInfo) extends Expr[AnyRef] {
      override def tpe: Type.Aux[AnyRef] = Type.Ref(classInfo)
      override def toSyntax: String      = s"error: uninitialized class ${classInfo.name}"
    }

    case class ConstTpe[A](tpe: Type.Aux[A], value: A) extends Expr[A] {
      override def toSyntax: String = s"($value: ${tpe.describe})"
    }

    case object Null extends Expr[AnyRef] {
      override def tpe: Type.Aux[AnyRef] = Type.AnyRef
      override def toSyntax: String      = "null"
    }

    case class Add[A](e1: Expr[A], e2: Expr[A]) extends Expr[A] {
      override def tpe: Type.Aux[A] = e1.tpe
      override def toSyntax: String = s"(${e1.toSyntax} + ${e2.toSyntax})"

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[A] =
        if (this == target) newExpr.asInstanceOf[Expr[A]]
        else Add(e1.substitute(target, newExpr), e2.substitute(target, newExpr))
    }
    case class Sub[A](e1: Expr[A], e2: Expr[A]) extends Expr[A] {
      override def tpe: Type.Aux[A] = e1.tpe
      override def toSyntax: String = s"(${e1.toSyntax} - ${e2.toSyntax})"

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[A] =
        if (this == target) newExpr.asInstanceOf[Expr[A]]
        else Sub(e1.substitute(target, newExpr), e2.substitute(target, newExpr))
    }
    case class Mult[A](e1: Expr[A], e2: Expr[A]) extends Expr[A] {
      override def tpe: Type.Aux[A] = e1.tpe
      override def toSyntax: String = s"(${e1.toSyntax} * ${e2.toSyntax})"

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[A] =
        if (this == target) newExpr.asInstanceOf[Expr[A]]
        else Mult(e1.substitute(target, newExpr), e2.substitute(target, newExpr))
    }
    case class Div[A](e1: Expr[A], e2: Expr[A]) extends Expr[A] {
      override def tpe: Type.Aux[A] = e1.tpe
      override def toSyntax: String = s"(${e1.toSyntax} / ${e2.toSyntax})"

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[A] =
        if (this == target) newExpr.asInstanceOf[Expr[A]]
        else Div(e1.substitute(target, newExpr), e2.substitute(target, newExpr))
    }
    case class Rem[A](e1: Expr[A], e2: Expr[A]) extends Expr[A] {
      override def tpe: Type.Aux[A] = e1.tpe
      override def toSyntax: String = s"(${e1.toSyntax} % ${e2.toSyntax})"

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[A] =
        if (this == target) newExpr.asInstanceOf[Expr[A]]
        else Rem(e1.substitute(target, newExpr), e2.substitute(target, newExpr))
    }
    case class Neg[A](e: Expr[A]) extends Expr[A] {
      override def tpe: Type.Aux[A] = e.tpe
      override def toSyntax: String = s"(-${e.toSyntax})"

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[A] =
        if (this == target) newExpr.asInstanceOf[Expr[A]]
        else Neg(e.substitute(target, newExpr))
    }
    case class ShiftLeft[A](e1: Expr[A], e2: Expr[Int]) extends Expr[A] {
      override def tpe: Type.Aux[A] = e1.tpe
      override def toSyntax: String = s"(${e1.toSyntax} << ${e2.toSyntax})"

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[A] =
        if (this == target) newExpr.asInstanceOf[Expr[A]]
        else ShiftLeft(e1.substitute(target, newExpr), e2.substitute(target, newExpr))
    }

    case class ShiftRight[A](e1: Expr[A], e2: Expr[Int]) extends Expr[A] {
      override def tpe: Type.Aux[A] = e1.tpe
      override def toSyntax: String = s"(${e1.toSyntax} >> ${e2.toSyntax})"

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[A] =
        if (this == target) newExpr.asInstanceOf[Expr[A]]
        else ShiftRight(e1.substitute(target, newExpr), e2.substitute(target, newExpr))
    }
    case class LogShiftRight[A](e1: Expr[A], e2: Expr[Int]) extends Expr[A] {
      override def tpe: Type.Aux[A] = e1.tpe
      override def toSyntax: String = s"(${e1.toSyntax} >>> ${e2.toSyntax})"

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[A] =
        if (this == target) newExpr.asInstanceOf[Expr[A]]
        else LogShiftRight(e1.substitute(target, newExpr), e2.substitute(target, newExpr))
    }

    case class And[A](e1: Expr[A], e2: Expr[A]) extends Expr[A] {
      override def tpe: Type.Aux[A] = e1.tpe
      override def toSyntax: String = s"(${e1.toSyntax} & ${e2.toSyntax})"

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[A] =
        if (this == target) newExpr.asInstanceOf[Expr[A]]
        else And(e1.substitute(target, newExpr), e2.substitute(target, newExpr))
    }
    case class Or[A](e1: Expr[A], e2: Expr[A]) extends Expr[A] {
      override def tpe: Type.Aux[A] = e1.tpe
      override def toSyntax: String = s"(${e1.toSyntax} | ${e2.toSyntax})"

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[A] =
        if (this == target) newExpr.asInstanceOf[Expr[A]]
        else Or(e1.substitute(target, newExpr), e2.substitute(target, newExpr))
    }
    case class Xor[A](e1: Expr[A], e2: Expr[A]) extends Expr[A] {
      override def tpe: Type.Aux[A] = e1.tpe
      override def toSyntax: String = s"(${e1.toSyntax} ^ ${e2.toSyntax})"

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[A] =
        if (this == target) newExpr.asInstanceOf[Expr[A]]
        else Xor(e1.substitute(target, newExpr), e2.substitute(target, newExpr))
    }

    case class Eq[A](e1: Expr[A], e2: Expr[A]) extends Expr[Boolean] {
      override def tpe: Type.Aux[Boolean] = Type.Boolean
      override def toSyntax: String       = s"(${e1.toSyntax} == ${e2.toSyntax})"

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[Boolean] =
        if (this == target) newExpr.asInstanceOf[Expr[Boolean]]
        else Eq(e1.substitute(target, newExpr), e2.substitute(target, newExpr))
    }
    case class Not(e: Expr[Boolean]) extends Expr[Boolean] {
      override def tpe: Type.Aux[Boolean] = Type.Boolean
      override def toSyntax: String       = s"!(${e.toSyntax})"

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[Boolean] =
        if (this == target) newExpr.asInstanceOf[Expr[Boolean]]
        else Not(e.substitute(target, newExpr))
    }
    case class LT(e1: Expr[Int], e2: Expr[Int]) extends Expr[Boolean] {
      override def tpe: Type.Aux[Boolean] = Type.Boolean
      override def toSyntax: String       = s"(${e1.toSyntax} < ${e2.toSyntax})"

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[Boolean] =
        if (this == target) newExpr.asInstanceOf[Expr[Boolean]]
        else LT(e1.substitute(target, newExpr), e2.substitute(target, newExpr))
    }
    case class GE(e1: Expr[Int], e2: Expr[Int]) extends Expr[Boolean] {
      override def tpe: Type.Aux[Boolean] = Type.Boolean
      override def toSyntax: String       = s"(${e1.toSyntax} >= ${e2.toSyntax})"

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[Boolean] =
        if (this == target) newExpr.asInstanceOf[Expr[Boolean]]
        else GE(e1.substitute(target, newExpr), e2.substitute(target, newExpr))
    }
    case class GT(e1: Expr[Int], e2: Expr[Int]) extends Expr[Boolean] {
      override def tpe: Type.Aux[Boolean] = Type.Boolean
      override def toSyntax: String       = s"(${e1.toSyntax} > ${e2.toSyntax})"

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[Boolean] =
        if (this == target) newExpr.asInstanceOf[Expr[Boolean]]
        else GT(e1.substitute(target, newExpr), e2.substitute(target, newExpr))
    }
    case class LE(e1: Expr[Int], e2: Expr[Int]) extends Expr[Boolean] {
      override def tpe: Type.Aux[Boolean] = Type.Boolean
      override def toSyntax: String       = s"(${e1.toSyntax} <= ${e2.toSyntax})"

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[Boolean] =
        if (this == target) newExpr.asInstanceOf[Expr[Boolean]]
        else LE(e1.substitute(target, newExpr), e2.substitute(target, newExpr))
    }

    case class Convert[A](e: Expr[_], to: Type.Aux[A]) extends Expr[A] {
      override def tpe: Type.Aux[A] = to
      override def toSyntax: String = s"${e.toSyntax}.asInstanceOf[${to.describe}]"

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[A] =
        if (this == target) newExpr.asInstanceOf[Expr[A]]
        else Convert(e.substitute(target, newExpr), to)
    }

    case class Compare[A](e1: Expr[A], e2: Expr[A], nanBehavior: OPCode.NanBehavior) extends Expr[Int] {
      override def tpe: Type.Aux[Int] = Type.Int

      override def toSyntax: String = s"${e1.toSyntax}.compare(${e2.toSyntax})" //TODO: Account for nan behavior

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[Int] =
        if (this == target) newExpr.asInstanceOf[Expr[Int]]
        else Compare(e1.substitute(target, newExpr), e2.substitute(target, newExpr), nanBehavior)
    }

    case class GetLocal[A](index: Int, tpe: Type.Aux[A]) extends Expr[A] {
      override def toSyntax: String = s"(var_$index: ${tpe.describe})"
    }
    case class GetFakeLocal[A](index: Int, tpe: Type.Aux[A]) extends Expr[A] {
      override def toSyntax: String = s"(local_$index: ${tpe.describe})"
    }
    case class GetStackLocal[A](index: Int, jumpTarget: Long, tpe: Type.Aux[A]) extends Expr[A] {
      override def toSyntax: String = s"(stack_${index}_$jumpTarget: ${tpe.describe})"
    }
    case class GetField(e: Expr[_], fieldRefInfo: FieldRefInfo) extends Expr[Any] {
      override def tpe: Type.Aux[Any] =
        Type.fromDescriptor(fieldRefInfo.nameAndType.descriptor).asInstanceOf[Type.Aux[Any]]

      override def toSyntax: String = s"(${e.toSyntax}.${fieldRefInfo.nameAndType.name}: ${tpe.describe})"

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[Any] =
        if (this == target) newExpr.asInstanceOf[Expr[Any]]
        else GetField(e.substitute(target, newExpr), fieldRefInfo)
    }
    case class GetStatic(fieldRefInfo: FieldRefInfo) extends Expr[Any] {
      override def tpe: Type.Aux[Any] =
        Type.fromDescriptor(fieldRefInfo.nameAndType.descriptor).asInstanceOf[Type.Aux[Any]]

      override def toSyntax: String = s"(${fieldRefInfo.clazz.name}.${fieldRefInfo.nameAndType.name}: ${tpe.describe})"
    }

    case class Cast(e: Expr[_], classInfo: ClassInfo) extends Expr[AnyRef] {
      override def tpe: Type.Aux[AnyRef] = Type.Ref(classInfo)
      override def toSyntax: String      = s"${e.toSyntax}.asInstanceOf[${tpe.describe}]"

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[AnyRef] =
        if (this == target) newExpr.asInstanceOf[Expr[AnyRef]]
        else Cast(e.substitute(target, newExpr), classInfo)
    }

    case class IsInstanceOf(e: Expr[_], classInfo: ClassInfo) extends Expr[Int] {
      override def tpe: Type.Aux[Int] = Type.Int
      override def toSyntax: String   = s"${e.toSyntax}.isInstanceOf[${tpe.describe}]"

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[Int] =
        if (this == target) newExpr.asInstanceOf[Expr[Int]]
        else IsInstanceOf(e.substitute(target, newExpr), classInfo)
    }

    case class ArrayLength(e: Expr[Array[_]]) extends Expr[Int] {
      override def tpe: Type.Aux[Int] = Type.Int
      override def toSyntax: String   = s"${e.toSyntax}.length"

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[Int] =
        if (this == target) newExpr.asInstanceOf[Expr[Int]]
        else ArrayLength(e.substitute(target, newExpr))
    }

    case class GetArray[A](e: Expr[Array[A]], idx: Expr[Int], tpe: Type.Aux[A]) extends Expr[A] {
      override def toSyntax: String = s"(${e.toSyntax}[${idx.toSyntax}]: ${tpe.describe})"

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[A] =
        if (this == target) newExpr.asInstanceOf[Expr[A]]
        else GetArray(e.substitute(target, newExpr), idx.substitute(target, newExpr), tpe)
    }
  }

  sealed trait CallType
  object CallType {
    case object Virtual   extends CallType
    case object Static    extends CallType
    case object Special   extends CallType
    case object Interface extends CallType
  }

  case object Nop                        extends SIR
  case class MaybeInit(clazz: ClassInfo) extends SIR
  case class NotNull(expr: Expr[_]) extends SIR {
    override def substituteExpr[B](target: Expr[B], newExpr: Expr[B]): SIR =
      NotNull(expr.substitute(target, newExpr))
  }
  case class NotZero(expr: Expr[_]) extends SIR {
    override def substituteExpr[B](target: Expr[B], newExpr: Expr[B]): SIR =
      NotZero(expr.substitute(target, newExpr))
  }
  case class NotNegative(expr: Expr[Int]) extends SIR {
    override def substituteExpr[B](target: Expr[B], newExpr: Expr[B]): SIR =
      NotNegative(expr.substitute(target, newExpr))
  }
  case class SetLocal(index: Int, e: Expr[_]) extends SIR {
    override def substituteExpr[B](target: Expr[B], newExpr: Expr[B]): SIR =
      SetLocal(index, e.substitute(target, newExpr))
  }
  case class SetStackLocal(index: Int, pc: Long, e: Expr[_]) extends SIR {
    override def substituteExpr[B](target: Expr[B], newExpr: Expr[B]): SIR =
      SetStackLocal(index, pc, e.substitute(target, newExpr))
  }
  case class SetArray[A](arr: Expr[Array[A]], idx: Expr[Int], obj: Expr[A]) extends SIR {
    override def substituteExpr[B](target: Expr[B], newExpr: Expr[B]): SIR =
      SetArray(arr.substitute(target, newExpr), idx.substitute(target, newExpr), obj.substitute(target, newExpr))
  }
  case class SetField(e: Expr[_], f: Expr[_], fieldRefInfo: FieldRefInfo) extends SIR {
    override def substituteExpr[B](target: Expr[B], newExpr: Expr[B]): SIR =
      SetField(e.substitute(target, newExpr), f.substitute(target, newExpr), fieldRefInfo)
  }
  case class SetStatic(fieldRefInfo: FieldRefInfo, e: Expr[_]) extends SIR {
    override def substituteExpr[B](target: Expr[B], newExpr: Expr[B]): SIR =
      SetStatic(fieldRefInfo, e.substitute(target, newExpr))
  }
  case class New(varIndex: Int, clazz: ClassInfo, variables: Seq[Expr[_]]) extends SIR {
    override def substituteExpr[B](target: Expr[B], newExpr: Expr[B]): SIR =
      New(varIndex, clazz, variables.map(_.substitute(target, newExpr)))
  }
  case class CallSuper(e: Expr[_], variables: Seq[Expr[_]]) extends SIR {
    override def substituteExpr[B](target: Expr[B], newExpr: Expr[B]): SIR =
      CallSuper(e.substitute(target, newExpr), variables.map(_.substitute(target, newExpr)))
  }
  case class Call(
      varIndex: Int,
      callType: CallType,
      clazz: ClassInfo,
      name: String,
      descriptor: Descriptor.MethodDescriptor,
      obj: Option[Expr[_]],
      variables: Seq[Expr[_]]
  ) extends SIR {
    override def substituteExpr[B](target: Expr[B], newExpr: Expr[B]): SIR =
      Call(
        varIndex,
        callType,
        clazz,
        name,
        descriptor,
        obj.map(_.substitute(target, newExpr)),
        variables.map(_.substitute(target, newExpr))
      )
  }
  case class NewArray[A](varIndex: Int, size: Expr[Int], arrTpe: Type.Aux[A]) extends SIR {
    override def substituteExpr[B](target: Expr[B], newExpr: Expr[B]): SIR =
      NewArray(varIndex, size.substitute(target, newExpr), arrTpe)
  }
  case class NewMultiArray(varIndex: Int, tpe: Type.Aux[Array[_]], sizesExpr: Vector[Expr[Int]]) extends SIR {
    override def substituteExpr[B](target: Expr[B], newExpr: Expr[B]): SIR =
      NewMultiArray(varIndex, tpe, sizesExpr.map(_.substitute(target, newExpr)))
  }
  case class If(expr: Expr[Boolean], branchPC: Long) extends SIR {
    override def substituteExpr[B](target: Expr[B], newExpr: Expr[B]): SIR =
      If(expr.substitute(target, newExpr), branchPC)
  }
  case class Switch(expr: Expr[Int], defaultPC: Long, pairs: Vector[(Int, Long)]) extends SIR {
    override def substituteExpr[B](target: Expr[B], newExpr: Expr[B]): SIR =
      Switch(expr.substitute(target, newExpr), defaultPC, pairs)
  }
  case class Goto(branchPC: Long) extends SIR
  case class Return(expr: Option[Expr[_]]) extends SIR {
    override def substituteExpr[B](target: Expr[B], newExpr: Expr[B]): SIR =
      Return(expr.map(_.substitute(target, newExpr)))
  }
  case class MonitorEnter(e: Expr[_]) extends SIR {
    override def substituteExpr[B](target: Expr[B], newExpr: Expr[B]): SIR =
      MonitorEnter(e.substitute(target, newExpr))
  }
  case class MonitorExit(e: Expr[_]) extends SIR {
    override def substituteExpr[B](target: Expr[B], newExpr: Expr[B]): SIR =
      MonitorExit(e.substitute(target, newExpr))
  }

}
