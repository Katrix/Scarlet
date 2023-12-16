package scarlet.graph

import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._
import scalax.collection.{Graph, GraphTraversal}
import scarlet.graph.CFG.{SIRBlock, SIRStructuredBlock}
import scarlet.ir.SIR
import scarlet.ir.SIR.{BinaryOp, Expr, UnaryOp}

import scala.annotation.tailrec

object Structurer {

  def structureStart(cfg: CFG[SIRBlock]): SIRStructuredBlock = {
    val start = SIRStructuredBlock.UnprocessedBlock(cfg.start)
    val nodes = cfg.graph.nodes.map(SIRStructuredBlock.UnprocessedBlock(_))
    val edges = cfg.graph.edges.map(e => SIRStructuredBlock.UnprocessedBlock(e.from.value) ~> SIRStructuredBlock.UnprocessedBlock(e.to.value))

    structure(Graph.from(nodes, edges), start)
  }


  @tailrec
  def structure(graph: Graph[SIRStructuredBlock, DiEdge], start: SIRStructuredBlock, n: Int = 0): SIRStructuredBlock = {
    if (graph.nodes.size == 1) graph.nodes.head
    else if (n == 500) //TODO: Configurable
      SIRStructuredBlock.ErrorBlock(
        "Did not manage to structure code. Too many iterations",
        SIRStructuredBlock.ErrorData.CFGData(CFG(graph, start))
      )
    else {
      val dfsPostOrder =
        graph
          .innerNodeDownUpTraverser(graph.get(start), GraphTraversal.Parameters(GraphTraversal.DepthFirst))
          .iterator
          .collect { case (false, n) => n }

      dfsPostOrder
        .flatMap { node =>
          val backEdges: Set[graph.EdgeT] = findBackEdges(graph)(node)
          val headsOfCyclicRegions        = backEdges.map(_.to)

          if (headsOfCyclicRegions.contains(node)) {
            Some(structureCyclic(graph)(node, backEdges))
          } else {
            val dominates: Set[graph.NodeT] = dominatedBy(graph)(graph.get(start), node)

            if (dominates.size > 1) None
            else {
              val newBlock = structureAcyclic(graph)(node, dominates)

              val existingNodes = graph.nodes.filter(!dominates(_)).map(_.value)
              val existingEdges = graph.edges.filter(_.forall(!dominates(_))).map(e => e.from.value ~> e.to.value)

              val newInEdges  = node.diPredecessors.map(_.value ~> newBlock)
              val newOutEdges = dominates.flatMap(_.diSuccessors).map(newBlock ~> _.value)

              val updatedGraph = Graph.from(existingNodes ++ Set(newBlock), existingEdges ++ newInEdges ++ newOutEdges)
              Some(updatedGraph)
            }
          }
        }
        .nextOption() match {
        case Some(updatedGraph) => structure(updatedGraph, start, n + 1)
        case None =>
          SIRStructuredBlock.ErrorBlock(
            "Did not manage to structure code. No progress made",
            SIRStructuredBlock.ErrorData.CFGData(CFG(graph, start))
          )
      }
    }
  }

  def dominatedBy(graph: Graph[SIRStructuredBlock, DiEdge])(start: graph.NodeT, node: graph.NodeT): Set[graph.NodeT] = {
    val tree = CFG.dominatorTree(graph)(start)
    tree.get(node).outerNodeTraverser().toSet.map((v: SIRStructuredBlock) => graph.get(v))
  }

  def findBackEdges(graph: Graph[SIRStructuredBlock, DiEdge])(node: graph.NodeT): Set[graph.EdgeT] = {
    def dfsWgb(
        node: graph.NodeT,
        grays: Set[graph.NodeT],
        blacks: Set[graph.NodeT]
    ): (Set[graph.EdgeT], Set[graph.NodeT], Set[graph.NodeT]) = {
      val (acc, newGrays, newBlacks) =
        node.diSuccessors.foldLeft((Set.empty[graph.EdgeT], grays ++ Set(node), blacks)) {
          case (t @ (acc, currentGrays, currentBlacks), n) =>
            if (currentGrays(n))
              (acc ++ node.findOutgoingTo(n).toSet, currentGrays, currentBlacks)
            else if (!currentBlacks(n)) {
              val res = dfsWgb(n, currentGrays, currentBlacks)
              (res._1 ++ acc, res._2, res._3)
            } else t
        }

      (acc, newGrays, newBlacks ++ Set(node))
    }

    dfsWgb(node, Set.empty, Set.empty)._1
  }

  def structureAcyclic[N, E[+X] <: EdgeLikeIn[X]](graph: Graph[SIRStructuredBlock, DiEdge])(
      node: graph.NodeT,
      dominates: Set[graph.NodeT]
  ): SIRStructuredBlock = {
    val subgraph =
      graph
        .outerNodeTraverser(node, subgraphNodes = n => dominates(n))
        .toGraph

    val code = subgraph.topologicalSort
      .getOrElse(sys.error("impossible"))
      .withLayerOrdering(subgraph.NodeOrdering(SIRStructuredBlock.ordering.compare(_, _)))
      .foldLeft((Map.empty[subgraph.NodeT, BoolExpr], List.empty[SIRStructuredBlock])) {
        case ((existingReachingConditions, acc), node) =>
          val optReachingCondition = computeReachingCondition(subgraph)(node, existingReachingConditions)

          optReachingCondition match {
            case Some(reachingCondition) =>
              val codeBlock = removeJumps(node.value)
              val isTrivial = BoolExpr.isTriviallyTrue(reachingCondition)
              val block =
                if (isTrivial) codeBlock
                else SIRStructuredBlock.If(BoolExpr.toSirExpr(reachingCondition), codeBlock, None)

              (
                existingReachingConditions.updated(node, reachingCondition),
                block :: acc
              )
            case None =>
              (
                existingReachingConditions,
                SIRStructuredBlock.ErrorBlock(
                  "Missing data to structure blocks",
                  SIRStructuredBlock.ErrorData.ExistingBlock(node.value)
                ) :: acc
              )
          }
      }
      ._2

    if (code.lengthIs > 1)
      SIRStructuredBlock.Sequence(code)
    else if (code.nonEmpty)
      code.head
    else
      SIRStructuredBlock.Sequence(Nil) //Probably unreachable
  }

  def makeSingleEntrySuccessor(graph: Graph[SIRStructuredBlock, DiEdge])(
      node: graph.NodeT
  ): (Graph[SIRStructuredBlock, DiEdge], SIRStructuredBlock) = ???

  def structureCyclic(graph: Graph[SIRStructuredBlock, DiEdge])(
      node: graph.NodeT,
      backEdges: Set[graph.EdgeT]
  ): Graph[SIRStructuredBlock, DiEdge] = {
    val (newGraph, newNodeValue) = makeSingleEntrySuccessor(graph)(node)
    val newNode                  = newGraph.get(newNodeValue)
    val newBackEdges             = backEdges.map(e => newGraph.anyEdgeSelector(newGraph.get(e.from), newGraph.get(e.to)).get)

    ???
  }

  def ensureWelformedConditionExpression(e: Expr[_]): Unit =
    require(
      !e.isInstanceOf[Expr.GetFakeLocal[_]] && !e.isInstanceOf[Expr.GetStackLocal[_]] || !e.isInstanceOf[Expr.GetLocal],
      "Malformed condition expression"
    )

  def computeReachingCondition(
      subgraph: Graph[SIRStructuredBlock, DiEdge]
  )(node: subgraph.NodeT, existing: Map[subgraph.NodeT, BoolExpr]): Option[BoolExpr] =
    node.value match {
      case SIRStructuredBlock.UnprocessedBlock(SIRBlock.SIRCodeBasicBlock(nodeLeader, _)) =>
        val basic = node.diPredecessors.map { parent =>
          parent.value match {
            case SIRStructuredBlock.UnprocessedBlock(SIRBlock.SIRCodeBasicBlock(_, code)) =>
              val conditionsForParent = existing(parent)
              val conditionsForParentToThis = code.last._2.last match {
                case SIR.If(expr, branchPC) =>
                  ensureWelformedConditionExpression(expr)

                  if (branchPC == nodeLeader) BoolExpr.fromSirExpr(expr)
                  else BoolExpr.Not(BoolExpr.fromSirExpr(expr))

                case SIR.Switch(expr, _, pairs) =>
                  ensureWelformedConditionExpression(expr)

                  val boolExpr = BoolExpr.fromSirExpr(expr)
                  pairs
                    .find(_._2 == nodeLeader)
                    .map(_._1)
                    .fold(
                      pairs
                        .map(t => BoolExpr.NotEquals(boolExpr, BoolExpr.ConstInt(t._1)))
                        .fold(BoolExpr.True)(BoolExpr.And)
                    )(i => BoolExpr.Equals(boolExpr, BoolExpr.ConstInt(i)))

                case SIR.Goto(_) => BoolExpr.True
                case _           => BoolExpr.True
              }

              Some(BoolExpr.And(conditionsForParent, conditionsForParentToThis))

            case _ => None
          }
        }

        if (basic.forall(_.isDefined)) {
          Some(BoolExpr.simplify(basic.map(_.get).fold(BoolExpr.True)(BoolExpr.Or)))
        } else None
      case _ => None
    }

  def removeJumps(block: SIRStructuredBlock): SIRStructuredBlock = block //TODO: Do this at the end instead

  sealed trait BoolExpr
  object BoolExpr {
    case object True                                 extends BoolExpr
    case object False                                extends BoolExpr
    case class Var(innerExpr: SIR.Expr[_])           extends BoolExpr
    case class Not(e: BoolExpr)                      extends BoolExpr
    case class Or(e1: BoolExpr, e2: BoolExpr)        extends BoolExpr
    case class And(e1: BoolExpr, e2: BoolExpr)       extends BoolExpr
    case class Xor(e1: BoolExpr, e2: BoolExpr)       extends BoolExpr
    case class Equals(e1: BoolExpr, e2: BoolExpr)    extends BoolExpr
    case class NotEquals(e1: BoolExpr, e2: BoolExpr) extends BoolExpr
    case class ConstInt(i: Int)                      extends BoolExpr

    def fromSirExpr(e: SIR.Expr[_]): BoolExpr =
      e match {
        case Expr.ConstTpe(_, true)  => True
        case Expr.ConstTpe(_, false) => False
        case Expr.BinaryExpr(e1, e2, op, _) =>
          val be1 = fromSirExpr(e1)
          val be2 = fromSirExpr(e2)
          op match {
            case BinaryOp.BitAnd   => And(be1, be2)
            case BinaryOp.BitOr    => Or(be1, be2)
            case BinaryOp.Xor      => Xor(be1, be2)
            case BinaryOp.Equal    => Equals(be1, be2)
            case BinaryOp.NotEqual => NotEquals(be1, be2)
            case _                 => Var(e)
          }
        case Expr.UnaryExpr(e, op, _) =>
          op match {
            case UnaryOp.Not => Not(fromSirExpr(e))
            case UnaryOp.Neg => Var(e)
          }
        case _ => Var(e)
      }

    def toSirExpr(e: BoolExpr): SIR.Expr[_] = e match {
      case True           => SIR.Expr.ConstTpe(SIR.Type.Boolean, true)
      case False          => SIR.Expr.ConstTpe(SIR.Type.Boolean, false)
      case Var(innerExpr) => innerExpr
      case Not(e)         => SIR.Expr.UnaryExpr(toSirExpr(e), SIR.UnaryOp.Not, SIR.Type.Boolean)
      case Or(e1, e2)     => SIR.Expr.BinaryExpr(toSirExpr(e1), toSirExpr(e2), SIR.BinaryOp.BitOr, SIR.Type.Boolean)
      case And(e1, e2)    => SIR.Expr.BinaryExpr(toSirExpr(e1), toSirExpr(e2), SIR.BinaryOp.BitAnd, SIR.Type.Boolean)
      case Xor(e1, e2)    => SIR.Expr.BinaryExpr(toSirExpr(e1), toSirExpr(e2), SIR.BinaryOp.Xor, SIR.Type.Boolean)
      case Equals(e1, e2) => SIR.Expr.BinaryExpr(toSirExpr(e1), toSirExpr(e2), SIR.BinaryOp.Equal, SIR.Type.Boolean)
      case NotEquals(e1, e2) =>
        SIR.Expr.BinaryExpr(toSirExpr(e1), toSirExpr(e2), SIR.BinaryOp.NotEqual, SIR.Type.Boolean)
      case ConstInt(i) =>
        //Kind of iffy if we reach here, so we'll try to convert it into a boolean as best as we can
        toSirExpr(if (i == 0) False else True)
    }

    def simplify(e: BoolExpr): BoolExpr = {
      def singlePass(expr: BoolExpr): BoolExpr =
        expr match {
          case Not(Not(e)) => simplify(e)

          case Or(False, e1) => simplify(e1)
          case Or(e1, False) => simplify(e1)
          case And(True, e1) => simplify(e1)
          case And(e1, True) => simplify(e1)

          case Or(True, _)   => True
          case Or(_, True)   => True
          case And(False, _) => False
          case And(_, False) => False

          case Or(e1, e2) if e1 == e2  => simplify(e1)
          case And(e1, e2) if e1 == e2 => simplify(e1)

          case Or(e1, Not(e2)) if e1 == e2  => True
          case Or(Not(e1), e2) if e1 == e2  => True
          case And(e1, Not(e2)) if e1 == e2 => False
          case And(Not(e1), e2) if e1 == e2 => False

          case Not(Or(e1, e2))  => And(Not(simplify(e1)), Not(simplify(e2)))
          case Not(And(e1, e2)) => Or(Not(simplify(e1)), Not(simplify(e2)))

          case Xor(e1, e2) if e1 == e2    => False
          case Equals(e1, e2) if e1 == e2 => True
          case True                       => True
          case False                      => False
          case Var(_)                     => expr
          case Not(e)                     => simplify(e)
          case Or(e1, e2)                 => Or(simplify(e1), simplify(e2))
          case And(e1, e2)                => And(simplify(e1), simplify(e2))
          case Xor(e1, e2)                => Xor(simplify(e1), simplify(e2))
          case Equals(e1, e2)             => Equals(simplify(e1), simplify(e2))
          case NotEquals(e1, e2)          => NotEquals(simplify(e1), simplify(e2))
          case ConstInt(_)                => expr
        }

      @tailrec
      def inner(expr: BoolExpr, n: Int): BoolExpr =
        if (n == 100) expr
        else {
          val newExpr = singlePass(expr)
          if (newExpr != expr) inner(expr, n + 1)
          else newExpr
        }

      inner(e, 0)
    }

    def isTriviallyTrue(expr: BoolExpr): Boolean = simplify(expr) match {
      case True              => true
      case False             => false
      case Var(_)            => false
      case Not(e)            => !isTriviallyTrue(e)
      case Or(e1, e2)        => isTriviallyTrue(e1) || isTriviallyTrue(e2)
      case And(e1, e2)       => isTriviallyTrue(e1) && isTriviallyTrue(e2)
      case Xor(e1, e2)       => isTriviallyTrue(e1) ^ isTriviallyTrue(e2)
      case Equals(e1, e2)    => e1 == e2
      case NotEquals(e1, e2) => isTriviallyTrue(Not(Equals(e1, e2)))
      case ConstInt(_)       => false
    }
  }
}
