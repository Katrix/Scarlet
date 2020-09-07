package scarlet.graph

import scala.collection.immutable.LongMap

import scarlet.classfile.denormalized.opcodes.OPCode
import scarlet.classfile.denormalized.opcodes.OPCode._
import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._

/**
  * A CFG that flows from opcode to opcode. To be used to convert opcodes to [[scarlet.ir.SIR]].
  *
  * @param cfg The graph for all the program counters for the code.
  * @param jumpTargets All the jump targets of this graph.
  */
case class OPCodeCFG(cfg: Graph[Long, DiEdge], jumpTargets: Set[Long])
object OPCodeCFG {

  def create(code: LongMap[OPCode]): OPCodeCFG = {
    val (edges, jumpTargets) = code
      .sliding(2)
      .map(_.toSeq)
      .map {
        case Seq((l1, IntIfZero(_, branchPC)), (l2, _)) =>
          (Seq(l1 ~> branchPC, l1 ~> l2), Seq(branchPC, l2))
        case Seq((l1, IntIfCmp(_, branchPC)), (l2, _)) =>
          (Seq(l1 ~> branchPC, l1 ~> l2), Seq(branchPC, l2))
        case Seq((l1, RefIf(_, branchPC)), (l2, _)) => (Seq(l1 ~> branchPC, l1 ~> l2), Seq(branchPC, l2))
        case Seq((l1, RefIfCmp(_, branchPC)), (l2, _)) =>
          (Seq(l1 ~> branchPC, l1 ~> l2), Seq(branchPC, l2))
        case Seq((l1, Goto(branchPC)), (_, _)) => (Seq(l1 ~> branchPC), Seq(branchPC))
        case Seq((l1, Switch(defaultPC, pairs)), (_, _)) =>
          (l1 ~> defaultPC +: pairs.map(t => l1 ~> t._2), defaultPC +: pairs.map(_._2))
        case Seq((l1, _), (l2, _)) => (Seq(l1 ~> l2), Nil)
        case Seq((_, _))           => (Nil, Nil)
      }
      .toVector
      .unzip

    OPCodeCFG(Graph.from(code.keys, edges.flatten), jumpTargets.flatten.toSet)
  }
}
