case class Metric(
  position: ((Int, Int), Int),
  field: Field,
  piece: Piece,
  points: Int,
  combo: Int,
  round: Int) {

  type Block = (Int, Int)
  type Position = (Block, Int) // Origin, angle

  override
  def toString: String = {
    blockHeight + " " + holeCount + " " + lostInRound + " " + chimneyCount + " " + holeDepth + " " + position
  }

  private def blocksInRegion(top: Int, left: Int, bottom: Int, right: Int): Boolean = {
    field.blocks exists {case (row, col) => row <= top && row >= bottom && col >= left && col <= right}
  }

  // Need toList to avoid merging duplicate row numbers
  lazy val blockHeight = field.blocks.toList.map(_._1).sum

  lazy val distanceFromPreferredSide = piece.getDistanceFromPreferredSide(position, field.width)

  lazy val chimneyCount = {
    val blocks = for (row <- 0 until field.height; col <- 0 until field.width) yield (row, col)
    blocks count {case (row, col) => field.empty((row, col)) &&
      field.empty((row+1, col)) && !field.empty((row+1,col-1)) && !field.empty((row+1,col+1))
    }
  }

  private val spawnStartCol = (field.width - 4) / 2
  private val spawnEndCol = spawnStartCol + 3

  def loseInX(x: Int): Boolean = {
    val top = field.height - x
    blocksInRegion(top, 0, top, spawnStartCol-1) ||
      blocksInRegion(top-1, spawnStartCol, top-1, spawnEndCol) ||
      blocksInRegion(top, spawnEndCol+1, top, field.width-1)
  }

  private lazy val holes = {
    val colHoles = for (col <- 0 until field.width) yield {
      def empty(row: Int) = field.empty((row, col))
      (field.height-1 to 0 by -1) dropWhile empty filter empty map {(_, col)}
    }
    colHoles.flatten
  }

  lazy val holeDepth = {
    def depth(hole: Block): Int = {
      val (row, col) = hole
      (row until field.height) count {row => !field.empty((row, col))}
    }

    (holes map depth).sum
  }

  lazy val holeCount = holes.size

  lazy val lostInRound: Int = {
    val top = field.height - 1
    val lost = blocksInRegion(top+4, 0, top+1, field.width - 1) ||
    blocksInRegion(top, spawnStartCol, top, spawnEndCol)
    if (lost) round else 1000
  }


  def <(other: Metric): Boolean = {
    def boolField(f: Metric => Boolean): Metric => Int = {
      x => if (f(x)) 1 else 0
    }

    def largerIsBetter(f: Metric => Int): Metric => Int = {
      x => -1 * f(x)
    }

    val priorities: Seq[Metric => Int] = Seq(
      largerIsBetter(_.lostInRound),
      boolField(_.loseInX(1)),
      largerIsBetter(_.points),
      boolField(_.loseInX(2)),
      boolField(_.loseInX(3)),
      boolField(_.chimneyCount > 6),
      _.holeCount,
      _.chimneyCount,
      _.blockHeight,
      _.holeDepth,
      _.distanceFromPreferredSide.toInt)

    val unequal = priorities dropWhile {f => f(this) == f(other)}
    if (unequal.isEmpty)
      return false

    val f = unequal.head
    f(this) > f(other) // Inverted because for most, larger is worse
  }
}
