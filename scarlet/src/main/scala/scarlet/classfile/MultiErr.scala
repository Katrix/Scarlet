package scarlet.classfile

import cats.data.NonEmptyList
import scodec.Err

/**
  * A scodec Err that can contain multiple errors
  * @param errors The errors that this error contains
  * @param extraContext The extra content to add to this error
  */
final case class MultiErr(errors: NonEmptyList[Err], extraContext: List[String]) extends Err {
  override def message: String = errors.map(_.message).toList.mkString("\n")

  override def messageWithContext: String = errors.map(_.messageWithContext).toList.mkString("\n")

  override def context: List[String] = errors.toList.flatMap(_.context) ::: extraContext

  override def pushContext(ctx: String): Err = copy(extraContext = ctx :: extraContext)
}
object MultiErr {

  /**
    * Combine two [[Err]] values into a [[MultiErr]], collapsing them further
    * together if one of the values is already a [[MultiErr]].
    */
  def combine(e1: Err, e2: Err): MultiErr = (e1, e2) match {
    case (MultiErr(e1s, extraCtx1), MultiErr(e2s, extraCtx2)) => MultiErr(e2s ::: e1s, extraCtx2 ::: extraCtx1)
    case (MultiErr(e1s, extraCtx1), _)                        => MultiErr(e2 :: e1s, extraCtx1)
    case (_, MultiErr(e2s, extraCtx2))                        => MultiErr(e2s :+ e1, extraCtx2)
    case (_, _)                                               => MultiErr(NonEmptyList.of(e1, e2), Nil)
  }
}
