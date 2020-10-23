package scarlet.ir

import cats.arrow.FunctionK
import scarlet.classfile.denormalized.ConstantPoolEntry.{ClassInfo, FieldRefInfo}
import scarlet.classfile.denormalized.Descriptor
import scarlet.classfile.denormalized.attribute.MethodParameters
import scarlet.classfile.denormalized.opcodes.OPCode
import scarlet.ir.SIR.Expr

import scala.annotation.tailrec

/**
  * A stackless IR to help with not having to deal with the stack further into
  * the decompiler.
  *
  * Based on this paper: http://people.irisa.fr/David.Pichardie/papers/aplas10.pdf
  *
  * We do make a few big changes away from that paper though. The first is that
  * anything that reads from a mutable value assigns a new temp var, which is
  * what we push to the stack instead. This removes the need for substitution
  * when a mutable value is set after we have read it to the stack, but not used
  * it yet.
  */
sealed trait SIR {

  /**
    * Substitute the target with a new expression, for all the expressions this instruction stores.
    */
  def substituteExpr[B](target: Expr[B], newExpr: Expr[B]): SIR = this

  /**
    * Modify the expressions this OPCode contains.
    */
  def modifyExpr(f: FunctionK[Expr, Expr]): SIR = this
}
object SIR {

  class TempVar(val index: Int) extends AnyVal {
    def inc: TempVar = new TempVar(index + 1)
  }

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
    def toSyntax(implicit syntaxExtra: SyntaxExtra): String

    /**
      * Substitutes this expression, or it's children with a new expression.
      * @param target The expression to test against.
      * @param newExpr The new expression to use if this was equal to the target.
      */
    def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[A] =
      if (target == this) newExpr.asInstanceOf[Expr[A]] else this

    /**
      * Modify this expressions children.
      */
    def modifyChildren(f: FunctionK[Expr, Expr]): Expr[A] = this

    /**
      * If this expression contains the target
      */
    def contains(target: Expr[_]): Boolean = target == this
  }
  object Expr {
    case class UninitializedRef(atAddress: Long, classInfo: ClassInfo) extends Expr[AnyRef] {
      override def tpe: Type.Aux[AnyRef]                               = Type.Ref(classInfo)
      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String = s"error: uninitialized class ${classInfo.name}"
    }

    case class ConstTpe[A](tpe: Type.Aux[A], value: A) extends Expr[A] {
      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String = s"($value: ${tpe.describe})"
    }

    case object Null extends Expr[AnyRef] {
      override def tpe: Type.Aux[AnyRef]                               = Type.AnyRef
      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String = "null"
    }

    case class Add[A](e1: Expr[A], e2: Expr[A]) extends Expr[A] {
      override def tpe: Type.Aux[A]                                    = e1.tpe
      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String = s"(${e1.toSyntax} + ${e2.toSyntax})"

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[A] =
        if (this == target) newExpr.asInstanceOf[Expr[A]]
        else Add(e1.substitute(target, newExpr), e2.substitute(target, newExpr))

      override def modifyChildren(f: FunctionK[Expr, Expr]): Expr[A] =
        Add(f(e1), f(e2))
    }
    case class Sub[A](e1: Expr[A], e2: Expr[A]) extends Expr[A] {
      override def tpe: Type.Aux[A]                                    = e1.tpe
      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String = s"(${e1.toSyntax} - ${e2.toSyntax})"

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[A] =
        if (this == target) newExpr.asInstanceOf[Expr[A]]
        else Sub(e1.substitute(target, newExpr), e2.substitute(target, newExpr))

      override def modifyChildren(f: FunctionK[Expr, Expr]): Expr[A] =
        Sub(f(e1), f(e2))
    }
    case class Mult[A](e1: Expr[A], e2: Expr[A]) extends Expr[A] {
      override def tpe: Type.Aux[A]                                    = e1.tpe
      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String = s"(${e1.toSyntax} * ${e2.toSyntax})"

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[A] =
        if (this == target) newExpr.asInstanceOf[Expr[A]]
        else Mult(e1.substitute(target, newExpr), e2.substitute(target, newExpr))

      override def modifyChildren(f: FunctionK[Expr, Expr]): Expr[A] =
        Mult(f(e1), f(e2))
    }
    case class Div[A](e1: Expr[A], e2: Expr[A]) extends Expr[A] {
      override def tpe: Type.Aux[A]                                    = e1.tpe
      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String = s"(${e1.toSyntax} / ${e2.toSyntax})"

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[A] =
        if (this == target) newExpr.asInstanceOf[Expr[A]]
        else Div(e1.substitute(target, newExpr), e2.substitute(target, newExpr))

      override def modifyChildren(f: FunctionK[Expr, Expr]): Expr[A] =
        Div(f(e1), f(e2))
    }
    case class Rem[A](e1: Expr[A], e2: Expr[A]) extends Expr[A] {
      override def tpe: Type.Aux[A]                                    = e1.tpe
      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String = s"(${e1.toSyntax} % ${e2.toSyntax})"

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[A] =
        if (this == target) newExpr.asInstanceOf[Expr[A]]
        else Rem(e1.substitute(target, newExpr), e2.substitute(target, newExpr))

      override def modifyChildren(f: FunctionK[Expr, Expr]): Expr[A] =
        Rem(f(e1), f(e2))
    }
    case class Neg[A](e: Expr[A]) extends Expr[A] {
      override def tpe: Type.Aux[A]                                    = e.tpe
      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String = s"(-${e.toSyntax})"

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[A] =
        if (this == target) newExpr.asInstanceOf[Expr[A]]
        else Neg(e.substitute(target, newExpr))

      override def modifyChildren(f: FunctionK[Expr, Expr]): Expr[A] =
        Neg(f(e))
    }
    case class ShiftLeft[A](e1: Expr[A], e2: Expr[Int]) extends Expr[A] {
      override def tpe: Type.Aux[A]                                    = e1.tpe
      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String = s"(${e1.toSyntax} << ${e2.toSyntax})"

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[A] =
        if (this == target) newExpr.asInstanceOf[Expr[A]]
        else ShiftLeft(e1.substitute(target, newExpr), e2.substitute(target, newExpr))

      override def modifyChildren(f: FunctionK[Expr, Expr]): Expr[A] =
        ShiftLeft(f(e1), f(e2))
    }

    case class ShiftRight[A](e1: Expr[A], e2: Expr[Int]) extends Expr[A] {
      override def tpe: Type.Aux[A]                                    = e1.tpe
      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String = s"(${e1.toSyntax} >> ${e2.toSyntax})"

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[A] =
        if (this == target) newExpr.asInstanceOf[Expr[A]]
        else ShiftRight(e1.substitute(target, newExpr), e2.substitute(target, newExpr))

      override def modifyChildren(f: FunctionK[Expr, Expr]): Expr[A] =
        ShiftRight(f(e1), f(e2))
    }
    case class LogShiftRight[A](e1: Expr[A], e2: Expr[Int]) extends Expr[A] {
      override def tpe: Type.Aux[A]                                    = e1.tpe
      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String = s"(${e1.toSyntax} >>> ${e2.toSyntax})"

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[A] =
        if (this == target) newExpr.asInstanceOf[Expr[A]]
        else LogShiftRight(e1.substitute(target, newExpr), e2.substitute(target, newExpr))

      override def modifyChildren(f: FunctionK[Expr, Expr]): Expr[A] =
        LogShiftRight(f(e1), f(e2))
    }

    case class And[A](e1: Expr[A], e2: Expr[A]) extends Expr[A] {
      override def tpe: Type.Aux[A]                                    = e1.tpe
      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String = s"(${e1.toSyntax} & ${e2.toSyntax})"

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[A] =
        if (this == target) newExpr.asInstanceOf[Expr[A]]
        else And(e1.substitute(target, newExpr), e2.substitute(target, newExpr))

      override def modifyChildren(f: FunctionK[Expr, Expr]): Expr[A] =
        And(f(e1), f(e2))
    }
    case class Or[A](e1: Expr[A], e2: Expr[A]) extends Expr[A] {
      override def tpe: Type.Aux[A]                                    = e1.tpe
      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String = s"(${e1.toSyntax} | ${e2.toSyntax})"

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[A] =
        if (this == target) newExpr.asInstanceOf[Expr[A]]
        else Or(e1.substitute(target, newExpr), e2.substitute(target, newExpr))

      override def modifyChildren(f: FunctionK[Expr, Expr]): Expr[A] =
        Or(f(e1), f(e2))
    }
    case class Xor[A](e1: Expr[A], e2: Expr[A]) extends Expr[A] {
      override def tpe: Type.Aux[A]                                    = e1.tpe
      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String = s"(${e1.toSyntax} ^ ${e2.toSyntax})"

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[A] =
        if (this == target) newExpr.asInstanceOf[Expr[A]]
        else Xor(e1.substitute(target, newExpr), e2.substitute(target, newExpr))

      override def modifyChildren(f: FunctionK[Expr, Expr]): Expr[A] =
        Xor(f(e1), f(e2))
    }

    case class Eq[A](e1: Expr[A], e2: Expr[A]) extends Expr[Boolean] {
      override def tpe: Type.Aux[Boolean]                              = Type.Boolean
      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String = s"(${e1.toSyntax} == ${e2.toSyntax})"

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[Boolean] =
        if (this == target) newExpr.asInstanceOf[Expr[Boolean]]
        else Eq(e1.substitute(target, newExpr), e2.substitute(target, newExpr))

      override def modifyChildren(f: FunctionK[Expr, Expr]): Expr[Boolean] =
        Eq(f(e1), f(e2))
    }
    case class Not(e: Expr[Boolean]) extends Expr[Boolean] {
      override def tpe: Type.Aux[Boolean]                              = Type.Boolean
      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String = s"!(${e.toSyntax})"

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[Boolean] =
        if (this == target) newExpr.asInstanceOf[Expr[Boolean]]
        else Not(e.substitute(target, newExpr))

      override def modifyChildren(f: FunctionK[Expr, Expr]): Expr[Boolean] =
        Not(f(e))
    }
    case class LT(e1: Expr[Int], e2: Expr[Int]) extends Expr[Boolean] {
      override def tpe: Type.Aux[Boolean]                              = Type.Boolean
      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String = s"(${e1.toSyntax} < ${e2.toSyntax})"

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[Boolean] =
        if (this == target) newExpr.asInstanceOf[Expr[Boolean]]
        else LT(e1.substitute(target, newExpr), e2.substitute(target, newExpr))

      override def modifyChildren(f: FunctionK[Expr, Expr]): Expr[Boolean] =
        LT(f(e1), f(e2))
    }
    case class GE(e1: Expr[Int], e2: Expr[Int]) extends Expr[Boolean] {
      override def tpe: Type.Aux[Boolean]                              = Type.Boolean
      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String = s"(${e1.toSyntax} >= ${e2.toSyntax})"

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[Boolean] =
        if (this == target) newExpr.asInstanceOf[Expr[Boolean]]
        else GE(e1.substitute(target, newExpr), e2.substitute(target, newExpr))

      override def modifyChildren(f: FunctionK[Expr, Expr]): Expr[Boolean] =
        GE(f(e1), f(e2))
    }
    case class GT(e1: Expr[Int], e2: Expr[Int]) extends Expr[Boolean] {
      override def tpe: Type.Aux[Boolean]                              = Type.Boolean
      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String = s"(${e1.toSyntax} > ${e2.toSyntax})"

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[Boolean] =
        if (this == target) newExpr.asInstanceOf[Expr[Boolean]]
        else GT(e1.substitute(target, newExpr), e2.substitute(target, newExpr))

      override def modifyChildren(f: FunctionK[Expr, Expr]): Expr[Boolean] =
        GE(f(e1), f(e2))
    }
    case class LE(e1: Expr[Int], e2: Expr[Int]) extends Expr[Boolean] {
      override def tpe: Type.Aux[Boolean]                              = Type.Boolean
      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String = s"(${e1.toSyntax} <= ${e2.toSyntax})"

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[Boolean] =
        if (this == target) newExpr.asInstanceOf[Expr[Boolean]]
        else LE(e1.substitute(target, newExpr), e2.substitute(target, newExpr))

      override def modifyChildren(f: FunctionK[Expr, Expr]): Expr[Boolean] =
        LE(f(e1), f(e2))
    }

    case class Convert[A](e: Expr[_], to: Type.Aux[A]) extends Expr[A] {
      override def tpe: Type.Aux[A]                                    = to
      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String = s"${e.toSyntax}.asInstanceOf[${to.describe}]"

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[A] =
        if (this == target) newExpr.asInstanceOf[Expr[A]]
        else Convert(e.substitute(target, newExpr), to)

      override def modifyChildren(f: FunctionK[Expr, Expr]): Expr[A] =
        Convert(f(e), to)
    }

    case class Compare[A](e1: Expr[A], e2: Expr[A], nanBehavior: OPCode.NanBehavior) extends Expr[Int] {
      override def tpe: Type.Aux[Int] = Type.Int

      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String =
        s"${e1.toSyntax}.compare(${e2.toSyntax})" //TODO: Account for nan behavior

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[Int] =
        if (this == target) newExpr.asInstanceOf[Expr[Int]]
        else Compare(e1.substitute(target, newExpr), e2.substitute(target, newExpr), nanBehavior)

      override def modifyChildren(f: FunctionK[Expr, Expr]): Expr[Int] =
        Compare(f(e1), f(e2), nanBehavior)
    }

    case class GetFakeLocal[A](tempVar: TempVar, tpe: Type.Aux[A]) extends Expr[A] {
      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String = s"(local_${tempVar.index}: ${tpe.describe})"
    }
    case class GetStackLocal[A](index: Int, jumpTarget: Long, tpe: Type.Aux[A]) extends Expr[A] {
      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String =
        s"(stack_${index}_$jumpTarget: ${tpe.describe})"
    }

    case class IsInstanceOf(e: Expr[_], classInfo: ClassInfo) extends Expr[Int] {
      override def tpe: Type.Aux[Int]                                  = Type.Int
      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String = s"${e.toSyntax}.isInstanceOf[${tpe.describe}]"

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[Int] =
        if (this == target) newExpr.asInstanceOf[Expr[Int]]
        else IsInstanceOf(e.substitute(target, newExpr), classInfo)

      override def modifyChildren(f: FunctionK[Expr, Expr]): Expr[Int] =
        IsInstanceOf(f(e), classInfo)
    }

    case class ArrayLength(e: Expr[Array[_]]) extends Expr[Int] {
      override def tpe: Type.Aux[Int]                                  = Type.Int
      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String = s"${e.toSyntax}.length"

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[Int] =
        if (this == target) newExpr.asInstanceOf[Expr[Int]]
        else ArrayLength(e.substitute(target, newExpr))

      override def modifyChildren(f: FunctionK[Expr, Expr]): Expr[Int] =
        ArrayLength(f(e))
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
  case class NotZero(expr: Expr[_]) extends SIR {
    override def substituteExpr[B](target: Expr[B], newExpr: Expr[B]): SIR =
      NotZero(expr.substitute(target, newExpr))

    override def modifyExpr(f: FunctionK[Expr, Expr]): SIR =
      NotZero(f(expr))
  }
  case class GetLocal(tempVar: TempVar, index: Int) extends SIR
  case class SetLocal(index: Int, e: Expr[_]) extends SIR {
    override def substituteExpr[B](target: Expr[B], newExpr: Expr[B]): SIR =
      SetLocal(index, e.substitute(target, newExpr))

    override def modifyExpr(f: FunctionK[Expr, Expr]): SIR =
      SetLocal(index, f(e))
  }
  case class SetFakeLocal(tempVar: TempVar, e: Expr[_]) extends SIR {
    override def substituteExpr[B](target: Expr[B], newExpr: Expr[B]): SIR =
      SetFakeLocal(tempVar, e.substitute(target, newExpr))

    override def modifyExpr(f: FunctionK[Expr, Expr]): SIR =
      SetFakeLocal(tempVar, f(e))
  }
  case class SetStackLocal(index: Int, pc: Long, e: Expr[_]) extends SIR {
    override def substituteExpr[B](target: Expr[B], newExpr: Expr[B]): SIR =
      SetStackLocal(index, pc, e.substitute(target, newExpr))

    override def modifyExpr(f: FunctionK[Expr, Expr]): SIR =
      SetStackLocal(index, pc, f(e))
  }
  case class GetArray[A](tempVar: TempVar, arr: Expr[Array[A]], idx: Expr[Int]) extends SIR {
    override def substituteExpr[B](target: Expr[B], newExpr: Expr[B]): SIR =
      GetArray(tempVar, arr.substitute(target, newExpr), idx.substitute(target, newExpr))

    override def modifyExpr(f: FunctionK[Expr, Expr]): SIR =
      GetArray(tempVar, f(arr), f(idx))
  }
  case class SetArray[A](arr: Expr[Array[A]], idx: Expr[Int], obj: Expr[A]) extends SIR {
    override def substituteExpr[B](target: Expr[B], newExpr: Expr[B]): SIR =
      SetArray(arr.substitute(target, newExpr), idx.substitute(target, newExpr), obj.substitute(target, newExpr))

    override def modifyExpr(f: FunctionK[Expr, Expr]): SIR =
      SetArray(f(arr), f(idx), f(obj))
  }
  case class GetField(tempVar: TempVar, e: Expr[_], fieldRefInfo: FieldRefInfo) extends SIR {
    override def substituteExpr[B](target: Expr[B], newExpr: Expr[B]): SIR =
      GetField(tempVar, e.substitute(target, newExpr), fieldRefInfo)

    override def modifyExpr(f: FunctionK[Expr, Expr]): SIR =
      GetField(tempVar, f(e), fieldRefInfo)
  }
  case class SetField(e: Expr[_], f: Expr[_], fieldRefInfo: FieldRefInfo) extends SIR {
    override def substituteExpr[B](target: Expr[B], newExpr: Expr[B]): SIR =
      SetField(e.substitute(target, newExpr), f.substitute(target, newExpr), fieldRefInfo)

    override def modifyExpr(f: FunctionK[Expr, Expr]): SIR =
      SetField(f(e), f(this.f), fieldRefInfo)
  }
  case class GetStatic(tempVar: TempVar, fieldRefInfo: FieldRefInfo) extends SIR
  case class SetStatic(fieldRefInfo: FieldRefInfo, e: Expr[_]) extends SIR {
    override def substituteExpr[B](target: Expr[B], newExpr: Expr[B]): SIR =
      SetStatic(fieldRefInfo, e.substitute(target, newExpr))

    override def modifyExpr(f: FunctionK[Expr, Expr]): SIR =
      SetStatic(fieldRefInfo, f(e))
  }
  case class New(tempVar: TempVar, clazz: ClassInfo, variables: Seq[Expr[_]]) extends SIR {
    override def substituteExpr[B](target: Expr[B], newExpr: Expr[B]): SIR =
      New(tempVar, clazz, variables.map(_.substitute(target, newExpr)))

    override def modifyExpr(f: FunctionK[Expr, Expr]): SIR =
      New(tempVar, clazz, variables.map(f(_)))
  }
  case class CallSuper(e: Expr[_], variables: Seq[Expr[_]]) extends SIR {
    override def substituteExpr[B](target: Expr[B], newExpr: Expr[B]): SIR =
      CallSuper(e.substitute(target, newExpr), variables.map(_.substitute(target, newExpr)))

    override def modifyExpr(f: FunctionK[Expr, Expr]): SIR =
      CallSuper(f(e), variables.map(f(_)))
  }
  case class Call(
      tempVar: TempVar,
      callType: CallType,
      clazz: ClassInfo,
      name: String,
      descriptor: Descriptor.MethodDescriptor,
      obj: Option[Expr[_]],
      variables: Seq[Expr[_]]
  ) extends SIR {
    override def substituteExpr[B](target: Expr[B], newExpr: Expr[B]): SIR =
      Call(
        tempVar,
        callType,
        clazz,
        name,
        descriptor,
        obj.map(_.substitute(target, newExpr)),
        variables.map(_.substitute(target, newExpr))
      )

    override def modifyExpr(f: FunctionK[Expr, Expr]): SIR =
      Call(tempVar, callType, clazz, name, descriptor, obj.map(f(_)), variables.map(f(_)))
  }
  case class NewArray[A](tempVar: TempVar, size: Expr[Int], arrTpe: Type.Aux[A]) extends SIR {
    override def substituteExpr[B](target: Expr[B], newExpr: Expr[B]): SIR =
      NewArray(tempVar, size.substitute(target, newExpr), arrTpe)

    override def modifyExpr(f: FunctionK[Expr, Expr]): SIR =
      NewArray(tempVar, f(size), arrTpe)
  }
  case class NewMultiArray(tempVar: TempVar, tpe: Type.Aux[Array[_]], sizesExpr: Vector[Expr[Int]]) extends SIR {
    override def substituteExpr[B](target: Expr[B], newExpr: Expr[B]): SIR =
      NewMultiArray(tempVar, tpe, sizesExpr.map(_.substitute(target, newExpr)))

    override def modifyExpr(f: FunctionK[Expr, Expr]): SIR =
      NewMultiArray(tempVar, tpe, sizesExpr.map(f(_)))
  }
  case class If(expr: Expr[Boolean], branchPC: Long) extends SIR {
    override def substituteExpr[B](target: Expr[B], newExpr: Expr[B]): SIR =
      If(expr.substitute(target, newExpr), branchPC)

    override def modifyExpr(f: FunctionK[Expr, Expr]): SIR =
      If(f(expr), branchPC)
  }
  case class Switch(expr: Expr[Int], defaultPC: Long, pairs: Vector[(Int, Long)]) extends SIR {
    override def substituteExpr[B](target: Expr[B], newExpr: Expr[B]): SIR =
      Switch(expr.substitute(target, newExpr), defaultPC, pairs)

    override def modifyExpr(f: FunctionK[Expr, Expr]): SIR =
      Switch(f(expr), defaultPC, pairs)
  }
  case class Goto(branchPC: Long) extends SIR
  case class Return(expr: Option[Expr[_]]) extends SIR {
    override def substituteExpr[B](target: Expr[B], newExpr: Expr[B]): SIR =
      Return(expr.map(_.substitute(target, newExpr)))

    override def modifyExpr(f: FunctionK[Expr, Expr]): SIR =
      Return(expr.map(f(_)))
  }
  case class MonitorEnter(e: Expr[_]) extends SIR {
    override def substituteExpr[B](target: Expr[B], newExpr: Expr[B]): SIR =
      MonitorEnter(e.substitute(target, newExpr))

    override def modifyExpr(f: FunctionK[Expr, Expr]): SIR =
      MonitorEnter(f(e))
  }
  case class MonitorExit(e: Expr[_]) extends SIR {
    override def substituteExpr[B](target: Expr[B], newExpr: Expr[B]): SIR =
      MonitorExit(e.substitute(target, newExpr))

    override def modifyExpr(f: FunctionK[Expr, Expr]): SIR =
      MonitorExit(f(e))
  }
  case class Cast(tempVar: TempVar, e: Expr[_], to: Type) extends SIR {
    override def substituteExpr[B](target: Expr[B], newExpr: Expr[B]): SIR =
      Cast(tempVar, e.substitute(target, newExpr), to)

    override def modifyExpr(f: FunctionK[Expr, Expr]): SIR =
      Cast(tempVar, f(e), to)
  }

  case class SyntaxExtra(methodParams: Option[MethodParameters])

  def toSyntax(sir: SIR)(implicit syntaxExtra: SyntaxExtra): Seq[String] = sir match {
    case Nop              => Nil
    case MaybeInit(clazz) => Seq(s"${clazz.name}.<clinit>")
    case NotZero(expr)    => Seq(s"notZero(${expr.toSyntax})")
    case SetLocal(index, e) =>
      Seq(syntaxExtra.methodParams.flatMap(_.parameters.lift(index - 1)).flatMap(_.name) match {
        case Some(name) => s"var $name = ${e.toSyntax}"
        case None       => s"var var_$index = ${e.toSyntax}"
      })
    case SetStackLocal(index, pc, e) => Seq(s"var stack_${index}_$pc = ${e.toSyntax}")
    case SetArray(arr, idx, obj)     => Seq(s"${arr.toSyntax}[${idx.toSyntax}] = ${obj.toSyntax}")
    case SetField(e, f, fieldRefInfo) =>
      Seq(s"${e.toSyntax}.${fieldRefInfo.nameAndType.name} = (${f.toSyntax})")
    case SetStatic(fieldRefInfo, e) =>
      Seq(s"${fieldRefInfo.clazz.name}.${fieldRefInfo.nameAndType.name} = ${e.toSyntax}")
    case New(varIndex, clazz, variables) =>
      Seq(s"var local_$varIndex = new ${clazz.name}(${variables.map(_.toSyntax).mkString(", ")})")
    case CallSuper(e, variables) =>
      Seq(s"${e.toSyntax}.super(${variables.map(_.toSyntax).mkString(", ")})")
    case Call(varIndex, _, clazz, name, _, None, variables) =>
      Seq(s"var local_$varIndex = ${clazz.name}.$name(${variables.map(_.toSyntax).mkString(", ")})")
    case Call(varIndex, _, _, name, _, Some(obj), variables) =>
      Seq(s"var local_$varIndex = ${obj.toSyntax}.$name(${variables.map(_.toSyntax).mkString(", ")})")
    case NewArray(varIndex, size, arrTpe) =>
      Seq(s"var local_$varIndex = new ${arrTpe.describe}[${size.toSyntax}]")
    case NewMultiArray(varIndex, tpe, sizesExpr) =>
      @tailrec
      def underlyingTpe(tpe: Type.Aux[_]): Type = tpe match {
        case Type.Array(inner) => underlyingTpe(inner)
        case _                 => tpe
      }

      val dimensionsBrackets = sizesExpr.map(e => s"[${e.toSyntax}]").mkString

      Seq(s"var local_$varIndex = new ${underlyingTpe(tpe).describe}$dimensionsBrackets")
    case If(expr, branchPC) => Seq(s"if(${expr.toSyntax}) goto $branchPC")
    case Switch(expr, defaultPC, pairs) =>
      val gotos = pairs.map {
        case (offset, pc) => s"case $offset: goto $pc"
      }

      s"switch(${expr.toSyntax}) {" +: gotos :+ s"default: goto $defaultPC" :+ "}"
    case Goto(branchPC)     => Seq(s"goto $branchPC")
    case Return(Some(expr)) => Seq(s"return ${expr.toSyntax}")
    case Return(None)       => Seq("return")
    case MonitorEnter(e)    => Seq(s"${e.toSyntax}.synchronizedStart")
    case MonitorExit(e)     => Seq(s"${e.toSyntax}.synchronizedEnd")
  }
}
