class Field(blocks: Set[(Int, Int)], width: Int, height: Int) {
  type Block = (Int, Int)

  def empty(block: Block): Boolean = {
    inside(block) && !(blocks contains block)
  }

  def inside(block: Block): Boolean = {
    val (row, col) = block
    row < height && col >= 0 && col < width
  }

  def moveValid(move: Set[Block]): Boolean = {
    move forall empty
  }

  def +(newBlocks: Set[Block]): (Field, Int) = {
    val combined = blocks ++ newBlocks
    val groupedByRow = combined groupBy {_._1}
    val (cleared, kept) = groupedByRow partition {_._2.size == width}

    val clearedRows = cleared.keySet
    if (clearedRows.size == 0)
      return (new Field(combined, width, height), 0)

    def clearedRowsBelow(row: Int): Int = clearedRows count {_ < row}

    def moveDown(row: Int): Set[Block] = {
      val newRow = row - clearedRowsBelow(row)
      kept(row) map {case (row, col) => (newRow, col)}
    }

    val moved = kept.keysIterator flatMap moveDown

    (new Field(moved.toSet, width, height), clearedRows.size)
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
