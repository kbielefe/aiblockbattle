class Field(val blocks: Set[(Int, Int)], val width: Int, val height: Int) {
  type Block = (Int, Int)
  type Position = ((Int, Int), Int)

  def empty(block: Block): Boolean = {
    inside(block) && !(blocks contains block)
  }

  def inside(block: Block): Boolean = {
    val (row, col) = block
    row >= 0 && col >= 0 && col < width
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

  def getValidMoves(pieceName: Char): Vector[(Position, Field, Int)] = {
    Vector.empty[(Position, Field, Int)]
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
