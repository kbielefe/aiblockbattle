class MovedField(field: Field, blocks: Set[(Int, Int)]) {
  type Block = (Int, Int)
  private val beforeRowClear = for (row <- 0 until field.height; col <- 0 until field.width; if field.isClearableBlock((row, col))) yield (row, col)
  private val combined = (beforeRowClear ++ blocks).toSet
  private val clearedRows = (combined groupBy {_._1} filter {_._2.size == field.width} map {_._1}).toSet
  private def removeRow(accum: (Int, Set[Block]), row: Int): (Int, Set[Block]) = {
    val (offset, result) = accum
    if (clearedRows contains row)
      (offset + 1, result)
    else
      (offset, result ++ (combined filter {_._1 == row} map {case (x, y) => (x + offset, y)}))
  }

  val (clearCount, cleared) = (field.height - 1 to -4 by -1).foldLeft((0, Set[Block]()))(removeRow)

  def isEmpty(block: Block): Boolean = {
    val (row, col) = block

    if (row >= field.height || col < 0 || col >= field.width)
      return false

    !(cleared contains block) && !field.isSolidBlock(block)
  }

  def lostGame: Boolean = cleared exists {_._1 < 0}
}
