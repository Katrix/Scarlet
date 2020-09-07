package scarlet.classfile.denormalized

import cats.data.{NonEmptyList, ValidatedNel}
import cats.syntax.all._
import scarlet.classfile.MultiErr
import scarlet.classfile.denormalized.ConstantPoolEntry.Utf8Info
import scodec._
import scodec.codecs._
import shapeless.Typeable

/**
  * Represents the denormalized constant pool of a classfile.
  */
case class ConstantPool(entries: Vector[ConstantPoolEntry]) {

  /**
    * Gets whatever entry is located at a given location.
    */
  def getAny(index: Int): ValidatedNel[Err, ConstantPoolEntry] = {
    val realIdx = index - 1
    entries.lift(realIdx).toValidNel(Err(s"Invalid index $index into constant pool"))
  }

  /**
    * Get an entry of this constant pool with a specific type. If the entry at
    * the passed in index is not the right type, and error is returned.
    */
  def get[A <: ConstantPoolEntry](index: Int)(implicit tpe: Typeable[A]): ValidatedNel[Err, A] = {
    getAny(index).andThen { entry =>
      tpe
        .cast(entry)
        .toValidNel(
          Err(
            s"Tried to get ${tpe.describe} at index $index in constant " +
              s"pool, but found ${entry.getClass.getSimpleName}"
          )
        )
    }
  }

  /**
    * Get an entry of this constant pool with a specific type, and wrap it in
    * an attempt.
    *
    * @see [[get]]
    */
  def getAttempt[A <: ConstantPoolEntry: Typeable](index: Int): Attempt[A] =
    Attempt.fromEither(get[A](index).toEither.leftMap {
      case NonEmptyList(head, Nil) => head
      case nel                     => MultiErr(nel, Nil)
    })

  /**
    * A codec that will grab an [[Utf8Info]] from this constant pool, and unwrap it.
    */
  def constantUtf8Codec: Codec[String] = constantCodec[Utf8Info].xmap(_.string, Utf8Info)

  /**
    * A codec that will return None if the next short is 0, or an [[Utf8Info]] if not.
    *
    * @see [[constantUtf8Codec]]
    */
  def constantMaybeUtf8Codec: Codec[Option[String]] =
    constantMaybeCodec[Utf8Info].xmap(_.map(_.string), _.map(Utf8Info))

  /**
    * A codec that will grab an entry of a given type from this constant pool.
    */
  def constantCodec[A <: ConstantPoolEntry: Typeable]: Codec[A] = uint16.narrow[A](getAttempt[A], indexOf)

  /**
    * A codec that will return None if the next short is 0, or the given
    * constant pool type if not.
    *
    * @see [[constantCodec]]
    */
  def constantMaybeCodec[A <: ConstantPoolEntry: Typeable]: Codec[Option[A]] =
    uint16
      .narrow[Option[A]](i => if (i == 0) Attempt.successful(None) else getAttempt(i).map(Some(_)), _.fold(0)(indexOf))

  /**
    * Retrieves the index of a constant pool entry.
    */
  def indexOf(entry: ConstantPoolEntry): Int = entries.indexOf(entry)
}
