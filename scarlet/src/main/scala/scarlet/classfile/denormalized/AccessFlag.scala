package scarlet.classfile.denormalized

import enumeratum.values._
import scodec._
import scodec.codecs._

import scala.collection.immutable

/**
  * Different types of access flags and other misc flags found on classes, fields and methods.
  * @param value
  */
sealed abstract class AccessFlag(val value: Int) extends IntEnumEntry
object AccessFlag extends IntEnum[AccessFlag] {
  case object Public    extends AccessFlag(0x0001)
  case object Private   extends AccessFlag(0x0002)
  case object Protected extends AccessFlag(0x0004)
  case object Static    extends AccessFlag(0x0008)
  case object Final     extends AccessFlag(0x0010)
  case object Volatile  extends AccessFlag(0x0040)
  case object Transient extends AccessFlag(0x0080)
  case object Synthetic extends AccessFlag(0x0100)
  case object Enum      extends AccessFlag(0x4000)

  val values: immutable.IndexedSeq[AccessFlag] = findValues

  /**
    * Convert an int bit map into a set of access flags.
    */
  def fromInt(flags: Int): Set[AccessFlag] =
    values.filter(flag => (flags & flag.value) == flag.value).toSet

  val codec: Codec[Set[AccessFlag]] =
    uint16.xmap[Set[AccessFlag]](AccessFlag.fromInt, set => set.map(_.value).fold(0)(_ | _))
}
