case class Node(field: Field, piece: Char, nextPieces: String, points: Int, combo: Int)

class BlockTree(state: Node, maximizing: Boolean)
      extends Tree[((Int, Int), Int), Node, Int](state, maximizing) {
  type Position = ((Int, Int), Int)

  def moveLessThan(left: Position, right: Position): Boolean = {
    (left._1._1 + left._1._2 + left._2) < (right._1._1 + right._1._2 + right._2)
  }

  def scoreLessThan(left: Int, right: Int): Boolean = left < right
  def leaf: Boolean = false
  def generateScore: Int = 0

  private def newPoints(clearCount: Int): Int = {
    state.points + (if (clearCount == 4) 8 else clearCount) + (if (clearCount > 0) state.combo else 0)
  }

  private def newCombo(clearCount: Int): Int = {
    if (clearCount > 0) state.combo + 1 else 0
  }

  def generateChildren: Vector[Child] = {
    if (maximizing) {
      val moves = state.field.getValidMoves(state.piece)
      moves map {case (move, field, clearCount) => (move, new BlockTree(Node(field, state.piece, state.nextPieces, newPoints(clearCount), newCombo(clearCount)), false))}
    } else {
      state.nextPieces.map(piece => (((-1, -1), -1), new BlockTree(Node(state.field, piece, "IJLOSTZ", state.points, state.combo), true))).toVector
    }
  }
}
