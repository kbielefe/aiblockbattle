case class Node(pieceNames: Vector[Char], moves: Vector[((Int, Int), Int)])

object BlockMinimax extends Minimax[Node, Metric] {
  def lt(left: Metric, right: Metric): Boolean = {
    left < right
  }

  def terminal(node: Node): Boolean = false
  def heuristic(node: Node): Infinite = Infinity
  def getChildren(node: Node): List[Node] = List[Node]()
}
