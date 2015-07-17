import scala.annotation.tailrec

abstract class Minimax[Node, Score] {
  def lt(left: Score, right: Score): Boolean
  def terminal(node: Node): Boolean
  def heuristic(node: Node): Infinite
  def getChildren(node: Node): List[Node]
  def nextLevelMaximizes(node: Node): Boolean

  sealed abstract class Infinite {
    def max(other: Infinite): Infinite = {
      if (this.lte(other))
        other
      else
        this
    }

    def min(other: Infinite): Infinite = {
      if (this.lte(other))
        this
      else
        other
    }

    def lte(other: Infinite): Boolean

    def getScore: Score = {
      throw new Exception("No result found")
    }
  }

  object Infinity extends Infinite {
    def lte(other: Infinite): Boolean = false
  }

  object NegativeInfinity extends Infinite {
    def lte(other: Infinite): Boolean = true
  }

  case class Finite(score: Score) extends Infinite {
    def lte(other: Infinite): Boolean = other match {
      case Infinity           => true
      case NegativeInfinity   => false
      case Finite(otherScore) => score == otherScore || lt(score, otherScore)
    }

    override def getScore: Score = score
  }

  @tailrec
  private def childLoop(
    children:   List[Node],
    result:     Infinite,
    alpha:      Infinite,
    beta:       Infinite,
    depth:      Int,
    maximizing: Boolean,
    nextMax:    Boolean): Infinite = {

    if (children.isEmpty)
      return result

    val child :: tail = children
    val childResult = alphabeta(child, depth-1, alpha, beta, nextMax)

    val newResult = if (maximizing) result.max(childResult) else result.min(childResult)
    val newAlpha  = if (maximizing) alpha.max(newResult)    else alpha
    val newBeta   = if (maximizing) beta                    else beta.min(newResult)

    if (newBeta.lte(newAlpha))
      return newResult

    childLoop(tail, newResult, newAlpha, beta, depth, maximizing, nextMax)
  }

  private def alphabeta(node: Node, depth: Int, alpha: Infinite, beta: Infinite, maximizing: Boolean): Infinite = {
    if (depth == 0 || terminal(node))
      return heuristic(node)

    val children = getChildren(node)
    val nextMax = nextLevelMaximizes(node)
    val initialValue = if (maximizing) NegativeInfinity else Infinity

    childLoop(children, initialValue, alpha, beta, depth, maximizing, nextMax)
  }

  //TODO:  Add a time cutoff
  def search(node: Node, depth: Int): Score = {
    alphabeta(node, depth, NegativeInfinity, Infinity, true).getScore
  }
}
