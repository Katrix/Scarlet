package scarlet.ir

import cats.arrow.FunctionK
import cats.data.Tuple2K
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

  /**
    * Structured SIR. Contains no codes for jumps
    */
  sealed trait SSIR extends SIR {

    override def substituteExpr[B](target: Expr[B], newExpr: Expr[B]): SSIR = this

    override def modifyExpr(f: FunctionK[Expr, Expr]): SSIR = this
  }

  class TempVar(val index: Int) extends AnyVal {
    def inc: TempVar = new TempVar(index + 1)

    override def toString: String = index.toString
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

  type Const[A, B] = A

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

    def fold[B](base: B)(f: FunctionK[Tuple2K[Const[B, *], Expr, *], Const[B, *]]): B = base

    /**
      * If this expression contains the target
      */
    def contains(target: Expr[_]): Boolean =
      target == this || fold(false)(new FunctionK[Tuple2K[Const[Boolean, *], Expr, *], Const[Boolean, *]] {
        override def apply[B](fa: Tuple2K[Const[Boolean, *], Expr, B]): Const[Boolean, B] =
          fa.first || fa.second == target
      })
  }
  sealed abstract class BinaryOp(val symbol: String)
  object BinaryOp {
    case object Add                                     extends BinaryOp("+")
    case object Sub                                     extends BinaryOp("-")
    case object Mul                                     extends BinaryOp("*")
    case object Div                                     extends BinaryOp("/")
    case object Rem                                     extends BinaryOp("%")
    case object ShiftLeft                               extends BinaryOp("<<")
    case object ShiftRight                              extends BinaryOp(">>")
    case object LogShiftRight                           extends BinaryOp(">>>")
    case object And                                     extends BinaryOp("&")
    case object Or                                      extends BinaryOp("|")
    case object Xor                                     extends BinaryOp("^")
    case object Equal                                   extends BinaryOp("==")
    case object NotEqual                                extends BinaryOp("!=")
    case object LT                                      extends BinaryOp("<")
    case object LE                                      extends BinaryOp("<=")
    case object GE                                      extends BinaryOp(">=")
    case object GT                                      extends BinaryOp(">")
    case class Compare(nanBehavior: OPCode.NanBehavior) extends BinaryOp(s"compare($nanBehavior)")
  }
  sealed abstract class UnaryOp(val symbol: String)
  object UnaryOp {
    case object Not extends UnaryOp("!")
    case object Neg extends UnaryOp("-")
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

    case class BinaryExpr[A, E1, E2](e1: Expr[E1], e2: Expr[E2], op: BinaryOp, tpe: Type.Aux[A]) extends Expr[A] {
      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String = s"(${e1.toSyntax} ${op.symbol} ${e2.toSyntax})"

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[A] =
        if (this == target) newExpr.asInstanceOf[Expr[A]]
        else BinaryExpr(e1.substitute(target, newExpr), e2.substitute(target, newExpr), op, tpe)

      override def modifyChildren(f: FunctionK[Expr, Expr]): Expr[A] =
        BinaryExpr(f(e1), f(e2), op, tpe)

      override def fold[B](base: B)(f: FunctionK[Tuple2K[Const[B, *], Expr, *], Const[B, *]]): B = {
        val b0 = base
        val b1 = f(Tuple2K[Const[B, *], Expr, E1](b0, e1))
        val b2 = f(Tuple2K[Const[B, *], Expr, E2](b1, e2))
        b2
      }
    }

    case class UnaryExpr[A, E](e: Expr[E], op: UnaryOp, tpe: Type.Aux[A]) extends Expr[A] {
      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String = s"${op.symbol}(${e.toSyntax})"

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[A] =
        if (this == target) newExpr.asInstanceOf[Expr[A]]
        else UnaryExpr(e.substitute(target, newExpr), op, tpe)

      override def modifyChildren(f: FunctionK[Expr, Expr]): Expr[A] =
        UnaryExpr(f(e), op, tpe)

      override def fold[B](base: B)(f: FunctionK[Tuple2K[Const[B, *], Expr, *], Const[B, *]]): B =
        f(Tuple2K[Const[B, *], Expr, E](base, e))
    }

    case class Convert[A, E](e: Expr[E], to: Type.Aux[A]) extends Expr[A] {
      override def tpe: Type.Aux[A]                                    = to
      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String = s"${e.toSyntax}.asInstanceOf[${to.describe}]"

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[A] =
        if (this == target) newExpr.asInstanceOf[Expr[A]]
        else Convert(e.substitute(target, newExpr), to)

      override def modifyChildren(f: FunctionK[Expr, Expr]): Expr[A] =
        Convert(f(e), to)

      override def fold[B](base: B)(f: FunctionK[Tuple2K[Const[B, *], Expr, *], Const[B, *]]): B =
        f(Tuple2K[Const[B, *], Expr, E](base, e))
    }

    case class GetFakeLocal[A](tempVar: TempVar, tpe: Type.Aux[A]) extends Expr[A] {
      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String = s"(local_${tempVar.index}: ${tpe.describe})"
    }
    case class GetStackLocal[A](index: Int, jumpTarget: Long, tpe: Type.Aux[A]) extends Expr[A] {
      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String =
        s"(stack_${index}_$jumpTarget: ${tpe.describe})"
    }

    case class IsInstanceOf[E](e: Expr[E], classInfo: ClassInfo) extends Expr[Int] {
      override def tpe: Type.Aux[Int]                                  = Type.Int
      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String = s"${e.toSyntax}.isInstanceOf[${tpe.describe}]"

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[Int] =
        if (this == target) newExpr.asInstanceOf[Expr[Int]]
        else IsInstanceOf(e.substitute(target, newExpr), classInfo)

      override def modifyChildren(f: FunctionK[Expr, Expr]): Expr[Int] =
        IsInstanceOf(f(e), classInfo)

      override def fold[B](base: B)(f: FunctionK[Tuple2K[Const[B, *], Expr, *], Const[B, *]]): B =
        f(Tuple2K[Const[B, *], Expr, E](base, e))
    }

    case class ArrayLength[E](e: Expr[Array[E]]) extends Expr[Int] {
      override def tpe: Type.Aux[Int]                                  = Type.Int
      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String = s"${e.toSyntax}.length"

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[Int] =
        if (this == target) newExpr.asInstanceOf[Expr[Int]]
        else ArrayLength(e.substitute(target, newExpr))

      override def modifyChildren(f: FunctionK[Expr, Expr]): Expr[Int] =
        ArrayLength(f(e))

      override def fold[B](base: B)(f: FunctionK[Tuple2K[Const[B, *], Expr, *], Const[B, *]]): B =
        f(Tuple2K[Const[B, *], Expr, Array[E]](base, e))
    }

    case class GetLocal(index: Int) extends Expr[Nothing] {
      override def tpe: Type.Aux[Nothing] = Type.Unknown

      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String =
        syntaxExtra.methodParams.flatMap(_.parameters.lift(index - 1)).flatMap(_.name) match {
          case Some(name) => s"$name"
          case None       => s"var_$index"
        }
    }

    case class GetArray[A](arr: Expr[Array[A]], idx: Expr[Int]) extends Expr[A] {
      override def tpe: Type.Aux[A] = arr.tpe match {
        case Type.Array(tpe) => tpe
        case _               => Type.Unknown.asInstanceOf[Type.Aux[A]]
      }

      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String = s"${arr.toSyntax}[${idx.toSyntax}]"

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[A] =
        if (this == target) newExpr.asInstanceOf[Expr[A]]
        else GetArray(arr.substitute(target, newExpr), idx.substitute(target, newExpr))

      override def modifyChildren(f: FunctionK[Expr, Expr]): Expr[A] =
        GetArray(f(arr), f(idx))

      override def fold[B](base: B)(f: FunctionK[Tuple2K[Const[B, *], Expr, *], Const[B, *]]): B = {
        val b0 = base
        val b1 = f(Tuple2K[Const[B, *], Expr, Array[A]](b0, arr))
        val b2 = f(Tuple2K[Const[B, *], Expr, Int](b1, idx))
        b2
      }
    }

    case class GetField[E](e: Expr[E], fieldRefInfo: FieldRefInfo) extends Expr[Nothing] {
      override def tpe: Type.Aux[Nothing] = Type.Unknown

      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String =
        s"${e.toSyntax}.${fieldRefInfo.nameAndType.name}"

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[Nothing] =
        if (this == target) newExpr.asInstanceOf[Expr[Nothing]]
        else GetField(e.substitute(target, newExpr), fieldRefInfo)

      override def modifyChildren(f: FunctionK[Expr, Expr]): Expr[Nothing] =
        GetField(f(e), fieldRefInfo)

      override def fold[B](base: B)(f: FunctionK[Tuple2K[Const[B, *], Expr, *], Const[B, *]]): B =
        f(Tuple2K[Const[B, *], Expr, E](base, e))
    }

    case class GetStatic(fieldRefInfo: FieldRefInfo) extends Expr[Nothing] {
      override def tpe: Type.Aux[Nothing] = Type.Unknown

      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String =
        s"${fieldRefInfo.clazz.name}.${fieldRefInfo.nameAndType.name}"
    }

    case class New(clazz: ClassInfo, variables: Seq[Expr[_]]) extends Expr[scala.AnyRef] {
      override def tpe: Type.Aux[scala.AnyRef] = Type.Ref(clazz)

      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String =
        s"new ${clazz.name}(${variables.map(_.toSyntax).mkString(", ")})"

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[scala.AnyRef] =
        if (this == target) newExpr.asInstanceOf[Expr[scala.AnyRef]]
        else New(clazz, variables.map(_.substitute(target, newExpr)))

      override def modifyChildren(f: FunctionK[Expr, Expr]): Expr[scala.AnyRef] =
        New(clazz, variables.map(f(_)))

      override def fold[B](base: B)(f: FunctionK[Tuple2K[Const[B, *], Expr, *], Const[B, *]]): B =
        variables.foldLeft(base)((b, expr) => f(Tuple2K[Const[B, *], Expr, Any](b, expr.asInstanceOf[Expr[Any]])))
    }
    case class Call(
        callType: CallType,
        clazz: ClassInfo,
        name: String,
        descriptor: Descriptor.MethodDescriptor,
        obj: Option[Expr[_]],
        variables: Seq[Expr[_]]
    ) extends Expr[Nothing] {
      override def tpe: Type.Aux[Nothing] = Type.fromDescriptor(descriptor).asInstanceOf[Type.Aux[Nothing]]

      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String =
        obj match {
          case None      => s"${clazz.name}.$name(${variables.map(_.toSyntax).mkString(", ")})"
          case Some(obj) => s"${obj.toSyntax}.$name(${variables.map(_.toSyntax).mkString(", ")})"
        }

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[Nothing] =
        if (this == target) newExpr.asInstanceOf[Expr[Nothing]]
        else
          Call(
            callType,
            clazz,
            name,
            descriptor,
            obj.map(_.substitute(target, newExpr)),
            variables.map(_.substitute(target, newExpr))
          )

      override def modifyChildren(f: FunctionK[Expr, Expr]): Expr[Nothing] =
        Call(
          callType,
          clazz,
          name,
          descriptor,
          obj.map(f(_)),
          variables.map(f(_))
        )

      override def fold[B](base: B)(f: FunctionK[Tuple2K[Const[B, *], Expr, *], Const[B, *]]): B = {
        val b1 = obj.fold(base)(e => f(Tuple2K[Const[B, *], Expr, Any](base, e.asInstanceOf[Expr[Any]])))
        variables.foldLeft(b1)((b, e) => f(Tuple2K[Const[B, *], Expr, Any](b, e.asInstanceOf[Expr[Any]])))
      }
    }
    case class NewArray[A](size: Expr[Int], arrTpe: Type.Aux[A]) extends Expr[Array[A]] {
      override def tpe: Type.Aux[Array[A]] = Type.Array(arrTpe)

      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String =
        s"new ${arrTpe.describe}[${size.toSyntax}]"

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[Array[A]] =
        if (this == target) newExpr.asInstanceOf[Expr[Array[A]]]
        else NewArray(size.substitute(target, newExpr), arrTpe)

      override def modifyChildren(f: FunctionK[Expr, Expr]): Expr[Array[A]] =
        NewArray(f(size), arrTpe)

      override def fold[B](base: B)(f: FunctionK[Tuple2K[Const[B, *], Expr, *], Const[B, *]]): B =
        f(Tuple2K[Const[B, *], Expr, Int](base, size))
    }
    case class NewMultiArray(tpe: Type.Aux[Array[_]], sizesExpr: Vector[Expr[Int]]) extends Expr[Array[_]] {
      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String = {
        @tailrec
        def underlyingTpe(tpe: Type.Aux[_]): Type = tpe match {
          case Type.Array(inner) => underlyingTpe(inner)
          case _                 => tpe
        }

        val dimensionsBrackets = sizesExpr.map(e => s"[${e.toSyntax}]").mkString

        s"new ${underlyingTpe(tpe).describe}$dimensionsBrackets"
      }

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[Array[_]] =
        if (this == target) newExpr.asInstanceOf[Expr[Array[_]]]
        else NewMultiArray(tpe, sizesExpr.map(_.substitute(target, newExpr)))

      override def modifyChildren(f: FunctionK[Expr, Expr]): Expr[Array[_]] =
        NewMultiArray(tpe, sizesExpr.map(f(_)))

      override def fold[B](base: B)(f: FunctionK[Tuple2K[Const[B, *], Expr, *], Const[B, *]]): B =
        sizesExpr.foldLeft(base)((b, expr) => f(Tuple2K[Const[B, *], Expr, Int](b, expr)))
    }
    case class Cast[A, E](e: Expr[E], tpe: Type.Aux[A]) extends Expr[A] {
      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String =
        s"${e.toSyntax}.asInstanceOf[${tpe.describe}]"

      override def substitute[B](target: Expr[B], newExpr: Expr[B]): Expr[A] =
        if (this == target) newExpr.asInstanceOf[Expr[A]]
        else Cast(e.substitute(target, newExpr), tpe)

      override def modifyChildren(f: FunctionK[Expr, Expr]): Expr[A] =
        Cast(f(e), tpe)

      override def fold[B](base: B)(f: FunctionK[Tuple2K[Const[B, *], Expr, *], Const[B, *]]): B =
        f(Tuple2K[Const[B, *], Expr, E](base, e))
    }
  }

  sealed trait CallType
  object CallType {
    case object Virtual   extends CallType
    case object Static    extends CallType
    case object Special   extends CallType
    case object Interface extends CallType
  }

  case object Nop                        extends SSIR
  case class MaybeInit(clazz: ClassInfo) extends SSIR
  case class SetLocal(index: Int, e: Expr[_]) extends SSIR {
    override def substituteExpr[B](target: Expr[B], newExpr: Expr[B]): SSIR =
      SetLocal(index, e.substitute(target, newExpr))

    override def modifyExpr(f: FunctionK[Expr, Expr]): SSIR =
      SetLocal(index, f(e))
  }
  case class IntVarIncr(index: Int, amount: Int) extends SSIR
  case class SetFakeLocal(tempVar: TempVar, e: Expr[_]) extends SSIR {
    override def substituteExpr[B](target: Expr[B], newExpr: Expr[B]): SSIR =
      SetFakeLocal(tempVar, e.substitute(target, newExpr))

    override def modifyExpr(f: FunctionK[Expr, Expr]): SSIR =
      SetFakeLocal(tempVar, f(e))
  }
  case class SetStackLocal(index: Int, pc: Long, e: Expr[_]) extends SSIR {
    override def substituteExpr[B](target: Expr[B], newExpr: Expr[B]): SSIR =
      SetStackLocal(index, pc, e.substitute(target, newExpr))

    override def modifyExpr(f: FunctionK[Expr, Expr]): SSIR =
      SetStackLocal(index, pc, f(e))
  }
  case class SetArray[A](arr: Expr[Array[A]], idx: Expr[Int], obj: Expr[A]) extends SSIR {
    override def substituteExpr[B](target: Expr[B], newExpr: Expr[B]): SSIR =
      SetArray(arr.substitute(target, newExpr), idx.substitute(target, newExpr), obj.substitute(target, newExpr))

    override def modifyExpr(f: FunctionK[Expr, Expr]): SSIR =
      SetArray(f(arr), f(idx), f(obj))
  }
  case class SetField(e: Expr[_], f: Expr[_], fieldRefInfo: FieldRefInfo) extends SSIR {
    override def substituteExpr[B](target: Expr[B], newExpr: Expr[B]): SSIR =
      SetField(e.substitute(target, newExpr), f.substitute(target, newExpr), fieldRefInfo)

    override def modifyExpr(f: FunctionK[Expr, Expr]): SSIR =
      SetField(f(e), f(this.f), fieldRefInfo)
  }
  case class SetStatic(fieldRefInfo: FieldRefInfo, e: Expr[_]) extends SSIR {
    override def substituteExpr[B](target: Expr[B], newExpr: Expr[B]): SSIR =
      SetStatic(fieldRefInfo, e.substitute(target, newExpr))

    override def modifyExpr(f: FunctionK[Expr, Expr]): SSIR =
      SetStatic(fieldRefInfo, f(e))
  }
  case class CallSuper(e: Expr[_], variables: Seq[Expr[_]]) extends SSIR {
    override def substituteExpr[B](target: Expr[B], newExpr: Expr[B]): SSIR =
      CallSuper(e.substitute(target, newExpr), variables.map(_.substitute(target, newExpr)))

    override def modifyExpr(f: FunctionK[Expr, Expr]): SSIR =
      CallSuper(f(e), variables.map(f(_)))
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
  case class Return(expr: Option[Expr[_]]) extends SSIR {
    override def substituteExpr[B](target: Expr[B], newExpr: Expr[B]): SSIR =
      Return(expr.map(_.substitute(target, newExpr)))

    override def modifyExpr(f: FunctionK[Expr, Expr]): SSIR =
      Return(expr.map(f(_)))
  }
  case class MonitorEnter(e: Expr[_]) extends SSIR {
    override def substituteExpr[B](target: Expr[B], newExpr: Expr[B]): SSIR =
      MonitorEnter(e.substitute(target, newExpr))

    override def modifyExpr(f: FunctionK[Expr, Expr]): SSIR =
      MonitorEnter(f(e))
  }
  case class MonitorExit(e: Expr[_]) extends SSIR {
    override def substituteExpr[B](target: Expr[B], newExpr: Expr[B]): SSIR =
      MonitorExit(e.substitute(target, newExpr))

    override def modifyExpr(f: FunctionK[Expr, Expr]): SSIR =
      MonitorExit(f(e))
  }
  case class Throw(e: Expr[_]) extends SSIR {
    override def substituteExpr[B](target: Expr[B], newExpr: Expr[B]): SSIR =
      Throw(e.substitute(target, newExpr))

    override def modifyExpr(f: FunctionK[Expr, Expr]): SSIR =
      Throw(f(e))
  }

  case class SyntaxExtra(methodParams: Option[MethodParameters])

  def toSyntax(sir: SIR)(implicit syntaxExtra: SyntaxExtra): Seq[String] = {
    sir match {
      case Nop              => Nil
      case MaybeInit(clazz) => Seq(s"classOf[${clazz.name}]")
      case SetLocal(index, e) =>
        Seq(syntaxExtra.methodParams.flatMap(_.parameters.lift(index - 1)).flatMap(_.name) match {
          case Some(name) => s"var $name = ${e.toSyntax}"
          case None       => s"var var_$index = ${e.toSyntax}"
        })
      case IntVarIncr(index, amount) =>
        Seq(syntaxExtra.methodParams.flatMap(_.parameters.lift(index - 1)).flatMap(_.name) match {
          case Some(name) => s"var $name = $name + $amount"
          case None       => s"var var_$index = var_$index + $amount"
        })
      case SetFakeLocal(tempVar, e)     => Seq(s"var local_${tempVar.index} = ${e.toSyntax}")
      case SetStackLocal(index, pc, e)  => Seq(s"var stack_${index}_$pc = ${e.toSyntax}")
      case SetArray(arr, idx, obj)      => Seq(s"${arr.toSyntax}[${idx.toSyntax}] = ${obj.toSyntax}")
      case SetField(e, f, fieldRefInfo) => Seq(s"${e.toSyntax}.${fieldRefInfo.nameAndType.name} = (${f.toSyntax})")
      case SetStatic(fieldRefInfo, e) =>
        Seq(s"${fieldRefInfo.clazz.name}.${fieldRefInfo.nameAndType.name} = ${e.toSyntax}")
      case CallSuper(e, variables) => Seq(s"${e.toSyntax}.super(${variables.map(_.toSyntax).mkString(", ")})")
      case If(expr, branchPC)      => Seq(s"if(${expr.toSyntax}) goto $branchPC")
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
      case Throw(e)           => Seq(s"throw ${e.toSyntax}")
    }
  }
}
