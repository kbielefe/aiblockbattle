/*
  val my_bot = state("your_bot")
  val field = Field(state(my_bot + "/field"))
  val combo = state(my_bot + "/combo").toInt
  val this_piece_type = state("game/this_piece_type")(0)
  val next_piece_type = state("game/next_piece_type")(0)
  val this_piece_position = state("game/this_piece_position") split ','
  val piece = pieces(this_piece_type)
  val next_piece = pieces(next_piece_type)
  val start = ((field.height - this_piece_position(1).toInt - piece.width, this_piece_position(0).toInt), 0)

    val boundaries = state.field.getBoundaries
    val potentialPositions = state.piece.getPositionsFromBoundaries(boundaries).toSet
    val potentialBlocks = potentialPositions map {position => (position, state.piece.getBlocksFromPosition(position))}
    val groupedBlocks = potentialBlocks groupBy {_._2} mapValues {_ map {_._1}}
    val validMoves = groupedBlocks filter {block => state.field.moveValid(block._1)}
*/

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
      moves map {case (move, field, clearCount) => (move, Node(field, piece, nextPiece, newPoints(clearCount), newCombo(clearCount)))}
    } else {
      state.nextPieces.map((((-1, -1), -1), Node(state.field, _, "IJLOSTZ", state.points, state.combo))).toVector
    }
  }
}
