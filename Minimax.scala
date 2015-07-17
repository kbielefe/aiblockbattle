class Minimax[Node](lt: (Node, Node) => Boolean) {
  sealed abstract class Score {
    def <(other: Score): Boolean

    def <=(other: Score): Boolean = {
      this < other || this == other
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
  }

  def alphabeta(node: Node, depth: Int, alpha: Score, beta: Score, maximizing: Boolean): Score = {
    Infinity
  }
}
