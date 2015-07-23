class Field(val blocks: Set[(Int, Int)], val width: Int, val height: Int) {
  type Block = (Int, Int)
  type Position = ((Int, Int), Int)

  def empty(block: Block): Boolean = {
    inside(block) && !(blocks contains block)
  }

  def inside(block: Block): Boolean = {
    val (row, col) = block
    row >= 0 && row < height + 3 && col >= 0 && col < width
  }

  def moveValid(move: Set[Block]): Boolean = {
    move forall empty
  }

  private def getClearedRows(combined: Set[Block]): (Set[Int], Map[Int, Set[Block]]) = {
    val groupedByRow = combined groupBy {_._1}
    val (cleared, kept) = groupedByRow partition {_._2.size == width}
    (cleared.keySet, kept)
  }

  private def getMovedRows(cleared: Set[Int], kept: Map[Int, Set[Block]]): Set[Block] = {
    def moveDown(row: Int): Set[Block] = {
      val newRow = row - (cleared count {_ < row})
      kept(row) map {case (row, col) => (newRow, col)}
    }

    (kept.keysIterator flatMap moveDown).toSet
  }

  def +(piece: Set[Block]): (Field, Int) = {
    val combined = blocks ++ piece

    val (cleared, kept) = getClearedRows(combined)
    val clearCount = cleared.size
    val newBlocks = if (clearCount == 0) combined else getMovedRows(cleared, kept)

    (new Field(newBlocks, width, height), clearCount)
  }

  private def above(block: Block): Block = block match {
    case (row, col) => (row+1, col)
  }

  private def below(block: Block): Block = block match {
    case (row, col) => (row-1, col)
  }

  def getBoundaries: Set[Block] = {
    val bottomBlocks = for (col <- 0 until width) yield (0, col)
    (blocks map above) ++ (bottomBlocks) filter empty
  }

  private def heuristic(start: Position, goal: Position): Double = {
    import math._

    val ((startX, startY), startAngle) = start
    val ((goalX, goalY), goalAngle) = goal
    val angleDiff = abs(AiBlockBattle.normalizeAngle(goalAngle - startAngle)) / 90
    val diffX = (goalX - startX).toDouble
    val diffY = (goalY - startY).toDouble

    diffX * diffX + diffY * diffY + angleDiff.toDouble
  }

  private def getNeighbors(piece: Piece)(position: Position): Set[Position] = {
    val ((row, col), angle) = position
    // Starts at bottom and moves up, because that will find unreachable positions the quickest
    Set(((row+1, col), angle),
        ((row, col-1), angle),
        ((row, col+1), angle),
        ((row, col), AiBlockBattle.normalizeAngle(angle - 90)),
        ((row, col), AiBlockBattle.normalizeAngle(angle + 90)))
  }

  private def reachableValid(piece: Piece)(position: Position): Boolean = {
    moveValid(piece.getBlocksFromPosition(position))
  }

  def getValidMoves(pieceName: Char): Vector[(Position, Field, Int)] = {
    val boundaries = getBoundaries
    val piece = Piece(pieceName)

    val reachable = new FastReachable(heuristic, getNeighbors(piece)_, reachableValid(piece)_)
    val goal = piece.getExpectedSpawnPosition(this)

    val potentialPositions = piece.getPositionsFromBoundaries(boundaries)
    val validPositions = potentialPositions filter {position => reachable.reachable(position, goal)}
    val combined = validPositions map {position => val (field, cleared) = this + piece.getBlocksFromPosition(position); (position, field, cleared)}
    combined.toVector
  }
}

object Field {
  def apply(string: String): Field = {
    val array = string split ';' map {_ split ',' map {_.toInt}}
    val width = array(0).size
    val height = array count {_(0) != 3}
    val blocks = for (row <- 0 until height; col <- 0 until width; if array(row)(col) == 2) yield (height-row-1, col)
    new Field(blocks.toSet, width, height)
  }
}
