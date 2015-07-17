class Minimax[Node](lt: (Node, Node) => Boolean, terminal: Node => Boolean, heuristic: Node => Score) {
  sealed abstract class Score {
    def <(other: Score): Boolean

    def <=(other: Score): Boolean = {
      this < other || this == other
    }

    def >(other: Score): Boolean = {
      !(this <= other)
    }

    def >=(other: Score): Boolean = {
      !(this < other)
    }

    def min(other: Score): Score = {
      if (this < other)
        this
      else
        other
    }

    def max(other: Score): Score = {
      if (this < other)
        other
      else
        this
    }

    def   <(other: Node): Boolean = this  < FiniteScore(other)
    def  <=(other: Node): Boolean = this <= FiniteScore(other)
    def   >(other: Node): Boolean = this  > FiniteScore(other)
    def  >=(other: Node): Boolean = this >= FiniteScore(other)
    def min(other: Node): Score = min(FiniteScore(other))
    def max(other: Node): Score = max(FiniteScore(other))

    def getNode: Node = {
      throw new Exception("Doesn't have a node")
    }
  }

  object Infinity extends Score {
    def <(other: Score): Boolean = false
  }

  object NegativeInfinity extends Score {
    def <(other: Score): Boolean = other match {
      case NegativeInfinity => false
      case _                => true
    }
  }

  case class FiniteScore(node: Node) extends Score {
    def <(other: Score): Boolean = other match {
      case NegativeInfinity => false
      case Infinity         => true
      case FiniteScore(otherNode) => lt(node, otherNode)
    }

    override def getNode: Node = node
  }

  def alphabeta(node: Node, depth: Int, alpha: Score, beta: Score, maximizing: Boolean): Score = {
    if (depth == 0 || terminal(node))
      return heuristic(node)
    Infinity
  }
}
