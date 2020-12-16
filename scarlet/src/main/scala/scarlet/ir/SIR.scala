package scarlet.ir

import cats.{Applicative, Monad}
import cats.syntax.all._
import scarlet.classfile.denormalized.ConstantPoolEntry.{ClassInfo, FieldRefInfo}
import scarlet.classfile.denormalized.Descriptor
import scarlet.classfile.denormalized.attribute.{Attribute, LocalVariableTable, MethodParameters}
import scarlet.classfile.denormalized.opcodes.OPCode
import scarlet.ir.SIR.Expr
import perspective._

import scala.annotation.tailrec
import scala.collection.immutable.TreeMap

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
    * Modify the direct expressions of this OPCode while maintaining some state.
    */
  def monoTraverse[G[_]: Applicative](f: Expr ~>: Compose2[G, Expr, *]): G[SIR] = (this: SIR).pure[G]

  /**
    * Modify the expressions of this OPCode while maintaining some state.
    */
  def monoTraverseDeep[G[_]: Monad](f: Expr ~>: Compose2[G, Expr, *]): G[SIR] = {
    lazy val deepTraverser: Expr ~>: Compose2[G, Expr, *] = new (Expr ~>: Compose2[G, Expr, *]) {
      override def apply[Z](expr: Expr[Z]): Compose2[G, Expr, Z] =
        expr.monoTraverse(deepTraverser).flatMap(e => f(e))
    }

    monoTraverse(deepTraverser)
  }

  /**
    * Fold the expressions this OPCode contains directly
    */
  def monoFoldLeftShallow[B](base: B)(f: Tuple2K[Const[B, *], Expr, *] ~>: Const[B, *]): B = base

  /**
    * Fold the expressions this OPCode contains
    */
  def monoFoldLeft[B](base: B)(f: Tuple2K[Const[B, *], Expr, *] ~>: Const[B, *]): B = {
    lazy val deepFolder: Tuple2K[Const[B, *], Expr, *] ~>: Const[B, *] =
      new (Tuple2K[Const[B, *], Expr, *] ~>: Const[B, *]) {
        override def apply[Z](fa: (Const[B, Z], Expr[Z])): Const[B, Z] = {
          val b1   = fa._1
          val expr = fa._2

          val b2 = expr.monoFoldLeftShallow(b1)(deepFolder)

          f((b2, expr))
        }
      }

    monoFoldLeftShallow(base)(deepFolder)
  }
}
object SIR {

  /**
    * Structured SIR. Contains no codes for jumps
    */
  sealed trait SSIR extends SIR

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
    case class Ref(classInfo: ClassInfo) extends BaseType[scala.AnyRef](classInfo.name.replace('/', '.'))

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
      if (target == this) newExpr.asInstanceOf[Expr[A]]
      else monoMap(Lambda[Expr ~>: Expr](_.substitute(target, newExpr)))

    /**
      * Modify this expressions children.
      */
    def monoMap(f: Expr ~>: Expr): Expr[A] = monoTraverse[cats.Id](f)

    /**
      * Fold this expression's direct children
      */
    def monoFoldLeftShallow[B](base: B)(f: Tuple2K[Const[B, *], Expr, *] ~>: Const[B, *]): B = base

    /**
      * Fold this expression's children
      */
    def monoFoldLeft[B](base: B)(f: Tuple2K[Const[B, *], Expr, *] ~>: Const[B, *]): B = {
      lazy val deepFolder: Tuple2K[Const[B, *], Expr, *] ~>: Const[B, *] =
        new (Tuple2K[Const[B, *], Expr, *] ~>: Const[B, *]) {
          override def apply[Z](fa: (Const[B, Z], Expr[Z])): Const[B, Z] = {
            val b1   = fa._1
            val expr = fa._2

            val b2 = expr.monoFoldLeftShallow(b1)(deepFolder)

            f((b2, expr))
          }
        }

      monoFoldLeftShallow(base)(deepFolder)
    }

    /**
      * Modify this expression's direct children while maintaining some state.
      */
    def monoTraverse[G[_]: Applicative](f: Expr ~>: Compose2[G, Expr, *]): G[Expr[A]] =
      (this: Expr[A]).pure[G]

    /**
      * Modify this expression's direct children while maintaining some state.
      */
    def monoTraverseDeep[G[_]: Monad](f: Expr ~>: Compose2[G, Expr, *]): G[Expr[A]] = {
      lazy val deepTraverser: Expr ~>: Compose2[G, Expr, *] = new (Expr ~>: Compose2[G, Expr, *]) {
        override def apply[Z](expr: Expr[Z]): Compose2[G, Expr, Z] =
          expr.monoTraverse(deepTraverser).flatMap(e => f(e))
      }

      monoTraverse(deepTraverser)
    }

    /**
      * If this expression contains the target
      */
    def contains(target: Expr[_]): Boolean =
      target == this || monoFoldLeftShallow(false)(
        Lambda[Tuple2K[Const[Boolean, *], Expr, *] ~>: Const[Boolean, *]](fa => fa._1 || fa._2 == target)
      )
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
    case object BitAnd                                  extends BinaryOp("&")
    case object BitOr                                   extends BinaryOp("|")
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
      override def tpe: Type.Aux[AnyRef] = Type.Ref(classInfo)
      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String =
        s"error: uninitialized class ${classInfo.name.replace('.', '/')}"
    }

    case class ConstTpe[A](tpe: Type.Aux[A], value: A) extends Expr[A] {
      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String = s"($value: ${tpe.describe})"
    }

    case object Null extends Expr[AnyRef] {
      override def tpe: Type.Aux[AnyRef]                               = Type.AnyRef
      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String = "null"
    }

    case class BinaryExpr[A](e1: Expr[_], e2: Expr[_], op: BinaryOp, tpe: Type.Aux[A]) extends Expr[A] {
      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String = s"(${e1.toSyntax} ${op.symbol} ${e2.toSyntax})"

      override def monoFoldLeftShallow[B](base: B)(f: Tuple2K[Const[B, *], Expr, *] ~>: Const[B, *]): B = {
        val b0 = base
        val b1 = f((b0, e1))
        val b2 = f((b1, e2))
        b2
      }

      override def monoTraverse[G[_]: Applicative](f: Expr ~>: Compose2[G, SIR.Expr, *]): G[Expr[A]] =
        (f(e1), f(e2)).mapN(BinaryExpr(_, _, op, tpe))
    }

    case class UnaryExpr[A](e: Expr[_], op: UnaryOp, tpe: Type.Aux[A]) extends Expr[A] {
      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String = s"${op.symbol}(${e.toSyntax})"

      override def monoFoldLeftShallow[B](base: B)(f: Tuple2K[Const[B, *], Expr, *] ~>: Const[B, *]): B =
        f((base, e))

      override def monoTraverse[G[_]: Applicative](f: Expr ~>: Compose2[G, SIR.Expr, *]): G[Expr[A]] =
        f(e).map(UnaryExpr(_, op, tpe))
    }

    case class Convert[A](e: Expr[_], to: Type.Aux[A]) extends Expr[A] {
      override def tpe: Type.Aux[A]                                    = to
      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String = s"${e.toSyntax}.asInstanceOf[${to.describe}]"

      override def monoFoldLeftShallow[B](base: B)(f: Tuple2K[Const[B, *], Expr, *] ~>: Const[B, *]): B =
        f((base, e))

      override def monoTraverse[G[_]: Applicative](f: Expr ~>: Compose2[G, SIR.Expr, *]): G[Expr[A]] =
        f(e).map(Convert(_, to))
    }

    case class GetFakeLocal[A](tempVar: TempVar, tpe: Type.Aux[A]) extends Expr[A] {
      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String = s"(local_${tempVar.index}: ${tpe.describe})"
    }
    case class GetStackLocal[A](index: Int, jumpTarget: Long, tpe: Type.Aux[A]) extends Expr[A] {
      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String =
        s"(stack_${index}_$jumpTarget: ${tpe.describe})"
    }

    case class IsInstanceOf(e: Expr[_], classInfo: ClassInfo) extends Expr[Boolean] {
      override def tpe: Type.Aux[Boolean]                              = Type.Boolean
      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String = s"${e.toSyntax}.isInstanceOf[${tpe.describe}]"

      override def monoFoldLeftShallow[B](base: B)(f: Tuple2K[Const[B, *], Expr, *] ~>: Const[B, *]): B =
        f((base, e))

      override def monoTraverse[G[_]: Applicative](f: Expr ~>: Compose2[G, SIR.Expr, *]): G[Expr[Boolean]] =
        f(e).map(IsInstanceOf(_, classInfo))
    }

    case class ArrayLength(e: Expr[Array[_]]) extends Expr[Int] {
      override def tpe: Type.Aux[Int]                                  = Type.Int
      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String = s"${e.toSyntax}.length"

      override def monoFoldLeftShallow[B](base: B)(f: Tuple2K[Const[B, *], Expr, *] ~>: Const[B, *]): B =
        f((base, e))

      override def monoTraverse[G[_]: Applicative](f: Expr ~>: Compose2[G, SIR.Expr, *]): G[Expr[Int]] =
        f(e).map(ArrayLength)
    }

    case class GetLocal(index: Int) extends Expr[Nothing] {
      override def tpe: Type.Aux[Nothing] = Type.Unknown

      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String =
        syntaxExtra.getVariableName(index)
    }

    case class GetArray[A](arr: Expr[Array[A]], idx: Expr[Int]) extends Expr[A] {
      override def tpe: Type.Aux[A] = arr.tpe match {
        case Type.Array(tpe) => tpe
        case _               => Type.Unknown.asInstanceOf[Type.Aux[A]]
      }

      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String = s"${arr.toSyntax}[${idx.toSyntax}]"

      override def monoFoldLeftShallow[B](base: B)(f: Tuple2K[Const[B, *], Expr, *] ~>: Const[B, *]): B = {
        val b0 = base
        val b1 = f((b0, arr))
        val b2 = f((b1, idx))
        b2
      }

      override def monoTraverse[G[_]: Applicative](f: Expr ~>: Compose2[G, SIR.Expr, *]): G[Expr[A]] =
        (f(arr), f(idx)).mapN(GetArray(_, _))
    }

    case class GetField[E](e: Expr[E], fieldRefInfo: FieldRefInfo) extends Expr[Nothing] {
      override def tpe: Type.Aux[Nothing] = Type.Unknown

      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String =
        s"${e.toSyntax}.${fieldRefInfo.nameAndType.name}"

      override def monoFoldLeftShallow[B](base: B)(f: Tuple2K[Const[B, *], Expr, *] ~>: Const[B, *]): B =
        f((base, e))

      override def monoTraverse[G[_]: Applicative](f: Expr ~>: Compose2[G, SIR.Expr, *]): G[Expr[Nothing]] =
        f(e).map(GetField(_, fieldRefInfo))
    }

    case class GetStatic(fieldRefInfo: FieldRefInfo) extends Expr[Nothing] {
      override def tpe: Type.Aux[Nothing] = Type.Unknown

      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String =
        s"${fieldRefInfo.clazz.name.replace('/', '.')}.${fieldRefInfo.nameAndType.name}"
    }

    case class New(clazz: ClassInfo, variables: Seq[Expr[_]]) extends Expr[scala.AnyRef] {
      override def tpe: Type.Aux[scala.AnyRef] = Type.Ref(clazz)

      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String =
        s"new ${clazz.name.replace('/', '.')}(${variables.map(_.toSyntax).mkString(", ")})"

      override def monoFoldLeftShallow[B](base: B)(f: Tuple2K[Const[B, *], Expr, *] ~>: Const[B, *]): B =
        variables.foldLeft(base)((b, expr) => f((b, expr)))

      override def monoTraverse[G[_]: Applicative](f: Expr ~>: Compose2[G, SIR.Expr, *]): G[Expr[AnyRef]] =
        variables.toVector.traverse[G, Expr[Any]](e => f(e.asInstanceOf[Expr[Any]])).map(New(clazz, _))
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
          case None      => s"${clazz.name.replace('/', '.')}.$name(${variables.map(_.toSyntax).mkString(", ")})"
          case Some(obj) => s"${obj.toSyntax}.$name(${variables.map(_.toSyntax).mkString(", ")})"
        }

      override def monoFoldLeftShallow[B](base: B)(f: Tuple2K[Const[B, *], Expr, *] ~>: Const[B, *]): B = {
        val b1 = obj.fold(base)(e => f((base, e)))
        variables.foldLeft(b1)((b, e) => f((b, e)))
      }

      override def monoTraverse[G[_]: Applicative](f: Expr ~>: Compose2[G, SIR.Expr, *]): G[Expr[Nothing]] = {
        val g1 = obj.traverse[G, Expr[Any]](e => f(e.asInstanceOf[Expr[Any]]))
        val g2 = variables.toVector.traverse[G, Expr[Any]](e => f(e.asInstanceOf[Expr[Any]]))

        (g1, g2).mapN(Call(callType, clazz, name, descriptor, _, _))
      }
    }
    case class NewArray[A](size: Expr[Int], arrTpe: Type.Aux[A]) extends Expr[Array[A]] {
      override def tpe: Type.Aux[Array[A]] = Type.Array(arrTpe)

      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String =
        s"new ${arrTpe.describe}[${size.toSyntax}]"

      override def monoFoldLeftShallow[B](base: B)(f: Tuple2K[Const[B, *], Expr, *] ~>: Const[B, *]): B =
        f((base, size))

      override def monoTraverse[G[_]: Applicative](f: Expr ~>: Compose2[G, SIR.Expr, *]): G[Expr[Array[A]]] =
        f(size).map(NewArray(_, arrTpe))
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

      override def monoFoldLeftShallow[B](base: B)(f: Tuple2K[Const[B, *], Expr, *] ~>: Const[B, *]): B =
        sizesExpr.foldLeft(base)((b, expr) => f((b, expr)))

      override def monoTraverse[G[_]: Applicative](f: Expr ~>: Compose2[G, SIR.Expr, *]): G[Expr[Array[_]]] =
        sizesExpr.traverse[G, Expr[Int]](e => f(e)).map(NewMultiArray(tpe, _))
    }
    case class Cast[A, E](e: Expr[E], tpe: Type.Aux[A]) extends Expr[A] {
      override def toSyntax(implicit syntaxExtra: SyntaxExtra): String =
        s"${e.toSyntax}.asInstanceOf[${tpe.describe}]"

      override def monoFoldLeftShallow[B](base: B)(f: Tuple2K[Const[B, *], Expr, *] ~>: Const[B, *]): B =
        f((base, e))

      override def monoTraverse[G[_]: Applicative](f: Expr ~>: Compose2[G, SIR.Expr, *]): G[Expr[A]] =
        f(e).map(Cast(_, tpe))
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
    override def monoFoldLeftShallow[B](base: B)(f: Tuple2K[Const[B, *], Expr, *] ~>: Const[B, *]): B =
      f((base, e))

    override def monoTraverse[G[_]: Applicative](f: Expr ~>: Compose2[G, SIR.Expr, *]): G[SIR] =
      f(e).map(SetLocal(index, _))
  }
  case class IntVarIncr(index: Int, amount: Int) extends SSIR
  case class SetFakeLocal(tempVar: TempVar, e: Expr[_]) extends SSIR {
    override def monoFoldLeftShallow[B](base: B)(f: Tuple2K[Const[B, *], Expr, *] ~>: Const[B, *]): B =
      f((base, e))

    override def monoTraverse[G[_]: Applicative](f: Expr ~>: Compose2[G, SIR.Expr, *]): G[SIR] =
      f(e).map(SetFakeLocal(tempVar, _))
  }
  case class ExecuteExpr(e: Expr[_]) extends SSIR {
    override def monoFoldLeftShallow[B](base: B)(f: Tuple2K[Const[B, *], Expr, *] ~>: Const[B, *]): B =
      f((base, e))

    override def monoTraverse[G[_]: Applicative](f: Expr ~>: Compose2[G, SIR.Expr, *]): G[SIR] =
      f(e).map(ExecuteExpr)
  }
  case class SetStackLocal(index: Int, pc: Long, e: Expr[_]) extends SSIR {
    override def monoFoldLeftShallow[B](base: B)(f: Tuple2K[Const[B, *], Expr, *] ~>: Const[B, *]): B =
      f((base, e))

    override def monoTraverse[G[_]: Applicative](f: Expr ~>: Compose2[G, SIR.Expr, *]): G[SIR] =
      f(e).map(SetStackLocal(index, pc, _))
  }
  case class SetArray[A](arr: Expr[Array[A]], idx: Expr[Int], obj: Expr[A]) extends SSIR {
    override def monoFoldLeftShallow[B](base: B)(f: Tuple2K[Const[B, *], Expr, *] ~>: Const[B, *]): B = {
      val b1 = f((base, idx))
      f((b1, obj))
    }

    override def monoTraverse[G[_]: Applicative](f: Expr ~>: Compose2[G, SIR.Expr, *]): G[SIR] =
      (f(arr), f(idx), f(obj)).mapN(SetArray(_, _, _))
  }
  case class SetField(e: Expr[_], f: Expr[_], fieldRefInfo: FieldRefInfo) extends SSIR {
    override def monoFoldLeftShallow[B](base: B)(f: Tuple2K[Const[B, *], Expr, *] ~>: Const[B, *]): B = {
      val b1 = f((base, e))
      f((b1, this.f))
    }

    override def monoTraverse[G[_]: Applicative](f: Expr ~>: Compose2[G, SIR.Expr, *]): G[SIR] =
      (f(e), f(this.f)).mapN(SetField(_, _, fieldRefInfo))
  }
  case class SetStatic(fieldRefInfo: FieldRefInfo, e: Expr[_]) extends SSIR {
    override def monoFoldLeftShallow[B](base: B)(f: Tuple2K[Const[B, *], Expr, *] ~>: Const[B, *]): B =
      f((base, e))

    override def monoTraverse[G[_]: Applicative](f: Expr ~>: Compose2[G, SIR.Expr, *]): G[SIR] =
      f(e).map(SetStatic(fieldRefInfo, _))
  }
  case class CallSuper(e: Expr[_], variables: Seq[Expr[_]]) extends SSIR {
    override def monoFoldLeftShallow[B](base: B)(f: Tuple2K[Const[B, *], Expr, *] ~>: Const[B, *]): B = {
      val b1 = f((base, e))
      variables.foldLeft(b1)((bn, e) => f((bn, e)))
    }

    override def monoTraverse[G[_]: Applicative](f: Expr ~>: Compose2[G, SIR.Expr, *]): G[SIR] = {
      //Avoid a warning here by capturing the existential
      def capture[A](e: Expr[A]): G[SIR] = {
        val g1 = f(e)
        val g2 = variables.toVector.traverse[G, Expr[Any]](e => f(e.asInstanceOf[Expr[Any]]))

        (g1, g2).mapN(CallSuper(_, _))
      }

      capture(e)
    }
  }
  case class If(expr: Expr[Boolean], branchPC: Long) extends SIR {
    override def monoFoldLeftShallow[B](base: B)(f: Tuple2K[Const[B, *], Expr, *] ~>: Const[B, *]): B =
      f((base, expr))

    override def monoTraverse[G[_]: Applicative](f: Expr ~>: Compose2[G, SIR.Expr, *]): G[SIR] =
      f(expr).map(If(_, branchPC))
  }
  case class Switch(expr: Expr[Int], defaultPC: Long, pairs: Vector[(Int, Long)]) extends SIR {
    override def monoFoldLeftShallow[B](base: B)(f: Tuple2K[Const[B, *], Expr, *] ~>: Const[B, *]): B =
      f((base, expr))

    override def monoTraverse[G[_]: Applicative](f: Expr ~>: Compose2[G, SIR.Expr, *]): G[SIR] =
      f(expr).map(Switch(_, defaultPC, pairs))
  }
  case class Goto(branchPC: Long) extends SIR
  case class Return(expr: Option[Expr[_]]) extends SSIR {
    override def monoFoldLeftShallow[B](base: B)(f: Tuple2K[Const[B, *], Expr, *] ~>: Const[B, *]): B =
      expr.fold(base)(e => f((base, e)))

    override def monoTraverse[G[_]: Applicative](f: Expr ~>: Compose2[G, SIR.Expr, *]): G[SIR] =
      expr.traverse[G, Expr[Any]](e => f(e.asInstanceOf[Expr[Any]])).map(Return(_))
  }
  case class MonitorEnter(e: Expr[_]) extends SSIR {
    override def monoFoldLeftShallow[B](base: B)(f: Tuple2K[Const[B, *], Expr, *] ~>: Const[B, *]): B =
      f((base, e))

    override def monoTraverse[G[_]: Applicative](f: Expr ~>: Compose2[G, SIR.Expr, *]): G[SIR] =
      f(e).map(MonitorEnter)
  }
  case class MonitorExit(e: Expr[_]) extends SSIR {
    override def monoFoldLeftShallow[B](base: B)(f: Tuple2K[Const[B, *], Expr, *] ~>: Const[B, *]): B =
      f((base, e))

    override def monoTraverse[G[_]: Applicative](f: Expr ~>: Compose2[G, SIR.Expr, *]): G[SIR] =
      f(e).map(MonitorExit)
  }
  case class Throw(e: Expr[_]) extends SSIR {
    override def monoFoldLeftShallow[B](base: B)(f: Tuple2K[Const[B, *], Expr, *] ~>: Const[B, *]): B =
      f((base, e))

    override def monoTraverse[G[_]: Applicative](f: Expr ~>: Compose2[G, SIR.Expr, *]): G[SIR] =
      f(e).map(Throw)
  }

  case class SyntaxExtra(methodParams: Option[MethodParameters], localVariableTable: Seq[LocalVariableTable]) {

    def getVariableName(index: Int): String = {
      if (index == 0) "this"
      else {
        val paramName = methodParams
          .flatMap(_.parameters.lift(index - 1))
          .flatMap(_.name)

        lazy val localName = localVariableTable.flatMap(_.localVariables).find(_.index == index).map(_.name)

        paramName.orElse(localName).getOrElse(s"var_$index")
      }
    }
  }
  object SyntaxExtra {
    def fromAttributes(attributes: Vector[Attribute]): SyntaxExtra = SyntaxExtra(
      attributes.collectFirst {
        case m: MethodParameters => m
      },
      attributes.collect {
        case t: LocalVariableTable => t
      }
    )

    val none: SyntaxExtra = SyntaxExtra(None, Nil)
  }

  def toSyntax(sir: SIR)(implicit syntaxExtra: SyntaxExtra): Seq[String] =
    sir match {
      case Nop              => Seq()
      case MaybeInit(clazz) => Seq(s"classOf[${clazz.name.replace('/', '.')}]")
      case SetLocal(index, e) =>
        val varName = syntaxExtra.getVariableName(index)
        Seq(s"var $varName = ${e.toSyntax}")
      case IntVarIncr(index, amount) =>
        val varName = syntaxExtra.getVariableName(index)
        Seq(s"var $varName = $varName + $amount")
      case SetFakeLocal(tempVar, e)     => Seq(s"var local_${tempVar.index} = ${e.toSyntax}")
      case ExecuteExpr(e)               => Seq(e.toSyntax)
      case SetStackLocal(index, pc, e)  => Seq(s"var stack_${index}_$pc = ${e.toSyntax}")
      case SetArray(arr, idx, obj)      => Seq(s"${arr.toSyntax}[${idx.toSyntax}] = ${obj.toSyntax}")
      case SetField(e, f, fieldRefInfo) => Seq(s"${e.toSyntax}.${fieldRefInfo.nameAndType.name} = (${f.toSyntax})")
      case SetStatic(fieldRefInfo, e) =>
        Seq(s"${fieldRefInfo.clazz.name.replace('/', '.')}.${fieldRefInfo.nameAndType.name} = ${e.toSyntax}")
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

  def toSyntaxBlockList(code: TreeMap[Long, Vector[SIR]])(implicit syntaxExtra: SyntaxExtra): Seq[String] =
    code.flatMap {
      case (pc, innerCode) =>
        toSyntaxPC(pc, innerCode)
    }.toSeq

  def toSyntaxPC(pc: Long, code: Vector[SIR])(implicit syntaxExtra: SyntaxExtra): Vector[String] = {
    val lines = code.flatMap(SIR.toSyntax)
    lines match {
      case Vector()    => Vector()
      case Vector(one) => Vector(s"$pc: $one")
      case Vector(head, tail @ _*) =>
        s"$pc: $head" +: tail.toVector.map(line => " " * pc.toString.length + s": $line")
    }
  }
}
