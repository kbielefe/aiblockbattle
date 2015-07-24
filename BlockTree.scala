case class Node(round: Int, field: Field, position: ((Int, Int), Int), piece: Char, nextPieces: String, points: Int, combo: Int) {
  lazy val leaf: Boolean = {
    val spawnStartCol = (field.width - 4) / 2
    val spawnEndCol = spawnStartCol + 3
    val top = field.height - 1
    val blocks = Piece(piece).getBlocksFromPosition(position)
    blocks exists {case (row, col) => row > top || (row == top && col >= spawnStartCol && col <= spawnEndCol)}
  }
}

class BlockTree(state: Node, maximizing: Boolean)
      extends Tree[((Int, Int), Int), Node, Metric](state, maximizing) {
  type Position = ((Int, Int), Int)

  def moveLessThan(left: Position, right: Position): Boolean = {
    (left._1._1 + left._1._2 + left._2) < (right._1._1 + right._1._2 + right._2)
  }

  def scoreLessThan(left: Metric, right: Metric): Boolean = left < right
  def leaf: Boolean = state.leaf
  def generateScore: Metric = {
    Metric(state.position, state.field, Piece(state.piece), state.points, state.combo, state.round)
  }

  private def newPoints(clearCount: Int): Int = {
    state.points + (if (clearCount == 4) 8 else clearCount) + (if (clearCount > 0) state.combo else 0)
  }

  private def newCombo(clearCount: Int): Int = {
    if (clearCount > 0) state.combo + 1 else 0
  }

  def generateChildren: Vector[Child] = {
    if (maximizing) {
      val moves = state.field.getValidMoves(state.piece)
      moves map {case (move, field, clearCount) => (move, new BlockTree(Node(state.round, field, move, state.piece, state.nextPieces, newPoints(clearCount), newCombo(clearCount)), false))}
    } else {
      state.nextPieces.map(piece => (((-1, -1), -1), new BlockTree(Node(state.round + 1, state.field, ((-1, -1), -1), piece, "SZOJLTI", state.points, state.combo), true))).toVector
    }
  }
}
