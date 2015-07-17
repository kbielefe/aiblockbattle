class Field(string: String) {
  type Block = (Int, Int)

  private val array = string split ';' map {_ split ',' map {_.toInt}}
  val height = array.size
  val width = array(0).size

  def isEmpty(block: Block): Boolean = {
    val (row, col) = block

    if (row < 0)
      return true

    if (row >= height || col < 0 || col >= width)
      return false

    val cell = array(row)(col)
    cell == 0 || cell == 1
  }

  def isClearableBlock(block: Block): Boolean = {
    val (row, col) = block

    if (row < 0 || row >= height || col < 0 || col >= width)
      return false

    array(row)(col) == 2
  }

  def isSolidBlock(block: Block): Boolean = {
    val (row, col) = block

    if (row < 0 || row >= height || col < 0 || col >= width)
      return false

    array(row)(col) == 3
  }

  def moveValid(move: Set[Block]): Boolean = {
    move forall isEmpty
  }
}
