package scarlet.graph

import scalax.collection.GraphTraversal
import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._
import scalax.collection.edge.LDiEdge
import scarlet.Scarlet
import scarlet.classfile.denormalized.attribute.Code
import scarlet.classfile.denormalized.opcodes.OPCode
import scarlet.classfile.denormalized.opcodes.OPCode._
import scarlet.ir.OPCodeToSIR.CodeWithStack
import scarlet.ir.SIR

import scala.collection.immutable.{LongMap, TreeMap}
import scala.reflect.ClassTag

case class CFG[Elem](graph: Graph[Elem, DiEdge], start: Elem)

//https://www.ndss-symposium.org/ndss2015/ndss-2015-programme/no-more-gotos-decompilation-using-pattern-independent-control-flow-structuring-and-semantics/
object CFG {
  case class OPCodeBasicBlock(leader: Long, code: TreeMap[Long, OPCode], isCatch: Boolean)
  sealed trait SIRBlock
  object SIRBlock {
    case class SIRCodeBasicBlock(code: TreeMap[Long, Vector[SIR]])                              extends SIRBlock
    case class SIRErrorBlock(pc: Long, error: String, op: OPCode, codeWithStack: CodeWithStack) extends SIRBlock

    implicit val ordering: Ordering[SIRBlock] = {
      case (_: SIRErrorBlock, _: SIRErrorBlock)     => 0
      case (_: SIRErrorBlock, _: SIRCodeBasicBlock) => -1
      case (_: SIRCodeBasicBlock, _: SIRErrorBlock) => 1
      case (cx: SIRCodeBasicBlock, cy: SIRCodeBasicBlock) =>
        cx.code.firstKey.compare(cy.code.firstKey)
    }
  }

  sealed trait SIRStructuredBlock
  object SIRStructuredBlock {
    case class CodeBlock(code: TreeMap[Long, Vector[SIR.SSIR]])                              extends SIRStructuredBlock
    case class ErrorBlock(pc: Long, error: String, op: OPCode, codeWithStack: CodeWithStack) extends SIRStructuredBlock

    case class Sequence(blocks: Seq[SIRStructuredBlock]) extends SIRStructuredBlock
    case class If(cond: SIR.Expr[Boolean], ifTrue: SIRStructuredBlock, ifFalse: Option[SIRStructuredBlock])
        extends SIRStructuredBlock
    case class While(cond: SIR.Expr[Boolean], block: SIRStructuredBlock, isDoWhile: Boolean) extends SIRStructuredBlock
    case class EndlessLoop(block: SIRStructuredBlock)                                        extends SIRStructuredBlock
    case class Match(cases: Seq[(SIR.Expr[Boolean], SIRStructuredBlock)], default: SIRStructuredBlock)
        extends SIRStructuredBlock
  }

  private object Last {
    def unapply[A, B](arg: TreeMap[A, B]): Option[(A, B)] = arg.lastOption
  }
  private object First {
    def unapply[A, B](arg: TreeMap[A, B]): Option[(A, B)] = arg.headOption
  }

  //https://en.wikipedia.org/wiki/Basic_block
  //https://www.researchgate.net/publication/2645337_Analyzing_Control_Flow_in_Java_Bytecode
  def createFromOPCode(opCodes: LongMap[OPCode], optCode: Option[Code]): CFG[OPCodeBasicBlock] = {
    if (opCodes.isEmpty) CFG(Graph.empty, OPCodeBasicBlock(0, TreeMap.empty, isCatch = false))
    else {
      val code = optCode.get

      val gotoLeaders = opCodes.tail
        .sliding(2)
        .map(_.toSeq)
        .flatMap {
          case Seq((_, IntIfZero(_, branchPC)), (l2, _))   => Seq(branchPC, l2)
          case Seq((_, IntIfCmp(_, branchPC)), (l2, _))    => Seq(branchPC, l2)
          case Seq((_, RefIf(_, branchPC)), (l2, _))       => Seq(branchPC, l2)
          case Seq((_, RefIfCmp(_, branchPC)), (l2, _))    => Seq(branchPC, l2)
          case Seq((_, Goto(branchPC)), (l2, _))           => Seq(branchPC, l2)
          case Seq((_, Switch(defaultPC, pairs)), (l2, _)) => pairs.map(_._2) :+ defaultPC :+ l2
          case Seq((_, RefThrow), (l2, _))                 => Seq(l2)
          case Seq((_, Return(_)), (l2, _))                => Seq(l2)
          case _                                           => Nil
        }
        .toSeq

      val tryBlocks   = code.exceptions.map(_.startPc.toLong)
      val catchBlocks = code.exceptions.map(_.handlerPc.toLong)

      val catchBlocksSet = catchBlocks.toSet

      val leaders = ((opCodes.head._1 +: gotoLeaders.toVector) ++ tryBlocks ++ catchBlocks).distinct.sorted
      val basicBlocks = (
        leaders.sliding(2) ++ (if (leaders.length > 1) Vector(Vector(leaders.last)) else Nil)
      )
        .map {
          case Seq(current, next) => current -> opCodes.view.filterKeys(k => k >= current && k < next)
          case Seq(current)       => current -> opCodes.view.filterKeys(k => k >= current) //Last leader
        }
        .map {
          case (leader, opCodes) =>
            OPCodeBasicBlock(leader, TreeMap(opCodes.toSeq: _*), catchBlocksSet.contains(leader))
        }
        .toVector
        .sortBy(_.leader)

      def isUnconditionalBranch(opCode: OPCode) = opCode match {
        case _: Goto  => true
        case RefThrow => true
        case _        => false
      }

      def branchesFrom(code: OPCode): Seq[Long] = code match {
        case Goto(branchPC)           => Seq(branchPC)
        case IntIfZero(_, branchPC)   => Seq(branchPC)
        case IntIfCmp(_, branchPC)    => Seq(branchPC)
        case RefIf(_, branchPC)       => Seq(branchPC)
        case RefIfCmp(_, branchPC)    => Seq(branchPC)
        case Switch(defaultPC, pairs) => pairs.map(_._2) :+ defaultPC
        case _                        => Nil
      }

      val basicBlockProduct = for {
        b1 <- basicBlocks
        b2 <- basicBlocks
      } yield (b1, b2)

      val normalEdges = basicBlockProduct.flatMap {
        case (b1 @ OPCodeBasicBlock(_, Last((pc1, b1Op)), _), b2 @ OPCodeBasicBlock(_, First((pc2, _)), _))
            if pc1 + 1 == pc2 && !isUnconditionalBranch(b1Op) =>
          Seq(b1 ~> b2)
        case (b1 @ OPCodeBasicBlock(_, Last((_, b1Op)), _), b2 @ OPCodeBasicBlock(_, First((b2Addr, _)), _))
            if branchesFrom(b1Op).contains(b2Addr) =>
          Seq(b1 ~> b2)
        case _ => Nil
      }

      val tryCatchEdges = code.exceptions.flatMap { tryCatch =>
        val tryBlock   = basicBlocks.find(_.leader == tryCatch.startPc)
        val catchBlock = basicBlocks.find(_.leader == tryCatch.handlerPc)

        tryBlock.zip(catchBlock) match {
          case Some((b1, b2)) =>
            Some(b1 ~> b2)
          case None =>
            Scarlet.printer.pprintln(s"Could not manage to find both try and catch block for exception handler")
            Scarlet.printer.pprintln(tryCatch)
            None
        }
      }

      val graph = Graph.from(basicBlocks, (normalEdges ++ tryCatchEdges).distinct)
      val start = basicBlocks(0)

      if (!graph.isConnected) {
        val otherComponents = graph.componentTraverser().withSubgraph(nodes = _.value != start).toVector
        Scarlet.printer.pprintln(s"Found unreachable code")
        Scarlet.printer.pprintln(otherComponents)
      }

      CFG(graph, start)
    }
  }

  def sliceGraph[N, E[+X] <: EdgeLikeIn[X]](graph: Graph[N, E], head: N, sink: N): Graph[N, E] = {
    val headT = graph.get(head)
    val sinkT = graph.get(sink)

    headT.withSubgraph(nodes = _.hasSuccessor(sinkT)).toGraph
  }

  def sliceGraphMultiple[RN <: N, VN <: N, N, E[+X] <: EdgeLikeIn[X]](
      graph: Graph[RN, E],
      head: RN,
      sinks: Seq[RN],
      createVirtual: () => VN,
      createEdge: (RN, VN) => E[N]
  )(implicit tag: ClassTag[E[N]]): Graph[RN, E] = {
    val virtualSink  = createVirtual()
    val newEdges     = sinks.map(createEdge(_, virtualSink)).toSet
    val currentEdges = graph.edges.map(_.toOuter)

    val withVirtualEdge = Graph.from(graph.nodes.map(_.value), currentEdges ++ newEdges)
    val sliced          = sliceGraph[N, E](withVirtualEdge, head, virtualSink)

    Graph.from(sliced.nodes.map(_.value), sliced.edges.map(_.toOuter).diff(newEdges)).asInstanceOf[Graph[RN, E]]
  }

  def sccGraph[N](graph: Graph[N, DiEdge]): Graph[Graph[N, DiEdge], LDiEdge] = {
    import scalax.collection.edge.Implicits._
    import scalax.collection.immutable.Graph

    val strongComponents =
      graph
        .strongComponentTraverser(GraphTraversal.Parameters(kind = GraphTraversal.DepthFirst))
        .map(_.to(Graph))
        .toVector

    if (strongComponents.length > 1) {
      val sccEdges = for {
        g1 <- strongComponents
        g2 <- strongComponents
        if g1.ne(g2)
        n1 <- g1.nodes
        n2 <- g2.nodes
        if graph.contains(n1.value ~> n2.value)
      } yield (g1 ~+> g2)((n1, n2))

      Graph.from(strongComponents, sccEdges)
    } else {
      Graph.from(strongComponents, Vector.empty)
    }
  }

  def structure(cfg: CFG[SIRBlock]) = {
    val regions = sccGraph(cfg.graph)
    val sortedNodes = regions.topologicalSortByComponent.toVector.flatMap(
      _.getOrElse(sys.error("impossible"))
        .withLayerOrdering(regions.NodeOrdering { (n1, n2) =>
          val l1 = n1.value.nodes.view.map(_.value).minOption
          val l2 = n2.value.nodes.view.map(_.value).minOption
          implicitly[Ordering[Option[SIRBlock]]].compare(l1, l2)
        })
        .toVector
    )

    sortedNodes.map { regionNode =>
      val region = regionNode.value

      if (region.nodes.sizeIs == 1) {
        // Acyclic region
      } else {
        // Cyclic region
        val (transformedRegions, transformedNode) = transformSinglePredecessorSuccessor(regions, regionNode)
      }

      ???
    }

    ???
  }

  def structureAcyclic[N, E[+X] <: EdgeLikeIn[X]](graph: Graph[N, E]) = {
    graph.topologicalSort.map { order => }

    ???
  }
}
