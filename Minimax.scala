import scala.annotation.tailrec

class Minimax[Move, State, Score] {
  private abstract class Infinite {
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

    def getScore: Score = throw new Exception("No score available")
    def hasScore: Boolean = false
  }

  private object Infinity extends Infinite {
    def lte(other: Infinite): Boolean = false
  }

  private object NegativeInfinity extends Infinite {
    def lte(other: Infinite): Boolean = true
  }

  private case class Finite(tree: Tree[Move, State, Score]) extends Infinite {
    def lte(other: Infinite): Boolean = other match {
      case Infinity           => true
      case NegativeInfinity   => false
      case Finite(otherTree) => {
        val score = tree.getScore
        val otherScore = otherTree.getScore
        score == otherScore || tree.scoreLessThan(score, otherScore)
      }
    }

    override def getScore: Score = tree.getScore
    override def hasScore: Boolean = true
  }

  @tailrec
  private def childLoop(
    children:   Vector[(Move, Tree[Move, State, Score])],
    result:     Infinite,
    alpha:      Infinite,
    beta:       Infinite,
    depth:      Int,
    maximizing: Boolean,
    deadline:   Long): Infinite = {

    if (children.isEmpty || System.currentTimeMillis() >= deadline)
      return result

    val (move, child) = children.head
    val remaining  = children.tail

    val childResult = alphabeta(child, depth-1, alpha, beta, !maximizing, deadline)

    val newResult = if (maximizing) result.max(childResult) else result.min(childResult)
    val newAlpha  = if (maximizing) alpha.max(newResult)    else alpha
    val newBeta   = if (maximizing) beta                    else beta.min(newResult)

    if (newBeta.lte(newAlpha))
      return newResult

    childLoop(remaining, newResult, newAlpha, beta, depth, maximizing, deadline)
  }

  private def alphabeta(tree: Tree[Move, State, Score],
                        depth: Int,
                        alpha: Infinite,
                        beta: Infinite,
                        maximizing: Boolean,
                        deadline: Long): Infinite = {

    if (depth == 0 || tree.leaf)
      return Finite(tree)

    val initialValue = if (maximizing) NegativeInfinity else Infinity

    val score = childLoop(tree.getChildren, initialValue, alpha, beta, depth, maximizing, deadline)
    if (score.hasScore)
      tree.score = Some(score.getScore)
    score
  }

  def run(tree: Tree[Move, State, Score], depth: Int, time: Long): Move = {
    alphabeta(tree, depth, NegativeInfinity, Infinity, true, System.currentTimeMillis() + time)
    tree.getChildren.head._1
  }
}
