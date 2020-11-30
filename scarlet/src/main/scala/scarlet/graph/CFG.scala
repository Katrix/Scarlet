package scarlet.graph

import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._
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
  case class OPCodeBasicBlock(leader: Long, code: TreeMap[Long, OPCode])
  sealed trait SIRBlock
  object SIRBlock {
    case class SIRCodeBasicBlock(code: TreeMap[Long, Vector[SIR]])                              extends SIRBlock
    case class SIRErrorBlock(pc: Long, error: String, op: OPCode, codeWithStack: CodeWithStack) extends SIRBlock
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
    if (opCodes.isEmpty) CFG(Graph.empty, OPCodeBasicBlock(0, TreeMap.empty))
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

      val leaders = ((opCodes.head._1 +: gotoLeaders.toVector) ++ tryBlocks ++ catchBlocks).distinct.sorted
      val basicBlocks = (
        leaders.sliding(2) ++ (if (leaders.length > 1) Vector(Vector(leaders.last)) else Nil)
      )
        .map {
          case Seq(current, next) => current -> opCodes.view.filterKeys(k => k >= current && k < next)
          case Seq(current)       => current -> opCodes.view.filterKeys(k => k >= current) //Last leader
        }
        .map(t => t._1 -> TreeMap(t._2.toSeq: _*))
        .toVector
        .sortBy(_._1)
        .map(t => OPCodeBasicBlock(t._1, t._2))

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
        case (b1 @ OPCodeBasicBlock(_, Last((pc1, b1Op))), b2 @ OPCodeBasicBlock(_, First((pc2, _))))
            if pc1 + 1 == pc2 && !isUnconditionalBranch(b1Op) =>
          Seq(b1 ~> b2)
        case (b1 @ OPCodeBasicBlock(_, Last((_, b1Op))), b2 @ OPCodeBasicBlock(_, First((b2Addr, _))))
            if branchesFrom(b1Op).contains(b2Addr) =>
          Seq(b1 ~> b2)
        case _ => Nil
      }

      val tryCatchEdges = code.exceptions.flatMap { tryCatch =>
        val tryBlock = basicBlocks.find(_.code.head._1 == tryCatch.startPc)
        val catchBlock = basicBlocks.find(_.code.head._1 == tryCatch.handlerPc)

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
