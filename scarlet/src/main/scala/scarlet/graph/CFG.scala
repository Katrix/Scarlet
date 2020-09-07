package scarlet.graph

import scala.collection.immutable.{LongMap, TreeMap}
import scala.reflect.ClassTag

import scarlet.ir.SIR
import scarlet.ir.SIR._
import scarlet.graph.CFG.BasicBlock
import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._

case class CFG[Block <: BasicBlock](graph: Graph[Block, DiEdge], start: Block)

//https://www.ndss-symposium.org/ndss2015/ndss-2015-programme/no-more-gotos-decompilation-using-pattern-independent-control-flow-structuring-and-semantics/
object CFG {
  sealed trait BasicBlock
  case class CodeBasicBlock(code: TreeMap[Long, SIR]) extends BasicBlock

  private object Last {
    def unapply[A, B](arg: TreeMap[A, B]): Option[(A, B)] = arg.lastOption
  }
  private object First {
    def unapply[A, B](arg: TreeMap[A, B]): Option[(A, B)] = arg.headOption
  }

  //https://en.wikipedia.org/wiki/Basic_block
  //https://www.researchgate.net/publication/2645337_Analyzing_Control_Flow_in_Java_Bytecode
  def createFromSIR(code: LongMap[SIR]): CFG[CodeBasicBlock] = {
    val gotoLeaders = code.tail.sliding(2).map(_.toSeq).flatMap {
      case Seq((_, If(_, branchPC)), (l2, _))             => Seq(branchPC, l2)
      case Seq((_, Goto(branchPC)), (l2, _))              => Seq(branchPC, l2)
      case Seq((_, Switch(_, defaultPC, pairs)), (l2, _)) => pairs.map(_._2) :+ defaultPC :+ l2
      //case Seq((_, RefThrow), (l2, _))                    => Seq(l2) //TODO: Try catch stuff
      case Seq((_, Return(_)), (l2, _)) => Seq(l2)
      //TODO: All the instructions that can throw. Also all method calls
      case _ => Nil
    }.toSeq

    val leaders = (code.head._1 +: gotoLeaders.toVector).distinct.sorted
    val basicBlocks = (
      leaders.sliding(2) ++ (if (leaders.length > 1) Vector(Vector(leaders.last)) else Nil)
    )
      .map {
        case Seq(current, next) => current -> code.view.filterKeys(k => k >= current && k < next)
        case Seq(current)       => current -> code.view.filterKeys(k => k >= current) //Last leader
      }
      .map(t => t._1 -> TreeMap(t._2.toSeq: _*))
      .toVector
      .sortBy(_._1)
      .map(t => CodeBasicBlock(t._2))

    def isUnconditionalBranch(code: SIR) = code match {
      case _: Goto => true
      case _       => false
    }

    def branchesFrom(code: SIR): Seq[Long] = code match {
      case Goto(branchPC)              => Seq(branchPC)
      case If(_, branchPC)             => Seq(branchPC)
      case Switch(_, defaultPC, pairs) => pairs.map(_._2) :+ defaultPC
      case _                           => Nil
    }

    val basicBlockProduct = for {
      b1 <- basicBlocks
      b2 <- basicBlocks
    } yield (b1, b2)

    val edges = basicBlockProduct.flatMap {
      case (b1 @ CodeBasicBlock(Last((b1Addr, b1Op))), b2 @ CodeBasicBlock(First((b2Addr, _))))
          if (b1Addr - (b1Addr % 1000)) + 1000 == b2Addr && !isUnconditionalBranch(b1Op) =>
        Seq(b1 ~> b2)
      case (b1 @ CodeBasicBlock(Last((_, b1Op))), b2 @ CodeBasicBlock(First((b2Addr, _))))
          if branchesFrom(b1Op).contains(b2Addr) =>
        Seq(b1 ~> b2)
      case _ => Nil
    }

    val graph = Graph.from(basicBlocks, edges)
    val start = basicBlocks(0)

    if (!graph.isConnected) {
      val otherComponents = graph.componentTraverser().withSubgraph(nodes = _.value != start).toVector
      pprint.pprintln(s"Found unreachable code")
      pprint.pprintln(otherComponents)
    }

    CFG(graph, start)
  }

  def sliceGraph[N, E[+X] <: EdgeLikeIn[X]](graph: Graph[N, E], head: N, sink: N): Graph[N, E] = {
    val headT = graph.get(head)
    val sinkT = graph.get(sink)

    //TODO: Optimize?
    headT.withSubgraph(nodes = _.hasSuccessor(sinkT)).toGraph
  }

  def sliceGraphMultiple[RN <: N, VN <: N, N, E[+X] <: EdgeLikeIn[X]](
      graph: Graph[RN, E],
      head: RN,
      sinks: Seq[RN],
      createVirtual: () => VN,
      createGraph: (RN, VN) => E[N]
  )(implicit tag: ClassTag[E[N]]): Graph[RN, E] = {
    val virtualSink = createVirtual()
    val newEdges    = sinks.map(createGraph(_, virtualSink)).toSet

    val currentEdges   = graph.edges.map(_.toOuter)
    val withExtraEdges = Graph.from(graph.nodes.map(_.value), currentEdges ++ newEdges)
    val sliced         = sliceGraph[N, E](withExtraEdges, head, virtualSink)

    Graph.from(sliced.nodes.map(_.value), sliced.edges.map(_.toOuter).diff(newEdges)).asInstanceOf[Graph[RN, E]]
  }

  def structureAcyclic[N, E[+X] <: EdgeLikeIn[X]](graph: Graph[N, E]) = {
    graph.topologicalSort.map { order => }

    ???
  }
}
