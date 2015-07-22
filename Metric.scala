case class Metric(
  blocks: Set[(Int, Int)],
  positions: Set[((Int, Int), Int)],
  field: Field,
  piece: Piece,
  start: ((Int, Int), Int),
  combo: Int,
  val parentPaths: List[List[((Int, Int), Int)]]) {

  type Block = (Int, Int)
  type Position = (Block, Int) // Origin, angle

  override
  def toString: String = {
    blockHeight + " " + holeCount + " " + lostGame + " " + chimneyCount + " " + holeDepth + " " + positions.head
  }

  lazy val (movedField, clearCount) = field + blocks

  lazy val newCombo = if (clearCount > 0) 1 + combo else 0

  private def blocksInRegion(top: Int, left: Int, bottom: Int, right: Int): Boolean = {
    movedField.blocks exists {case (row, col) => row <= top && row >= bottom && col >= left && col <= right}
  }

  // Need toList to avoid merging duplicate row numbers
  lazy val blockHeight = blocks.toList.map(_._1).sum

  lazy val distanceFromPreferredSide = piece.getDistanceFromPreferredSide(positions.head, field.width)

  lazy val chimneyCount = {
    val blocks = for (row <- 0 until field.height; col <- 0 until field.width) yield (row, col)
    blocks count {case (row, col) => movedField.empty((row, col)) &&
      movedField.empty((row+1, col)) && !movedField.empty((row+1,col-1)) && !movedField.empty((row+1,col+1))
    }
  }

  private val spawnStartCol = (field.width - 4) / 2
  private val spawnEndCol = spawnStartCol + 3

  def loseInX(x: Int): Boolean = {
    val top = movedField.height - x
    blocksInRegion(top, 0, top, spawnStartCol-1) ||
      blocksInRegion(top-1, spawnStartCol, top-1, spawnEndCol) ||
      blocksInRegion(top, spawnEndCol+1, top, movedField.width-1)
  }

  private lazy val holes = {
    val colHoles = for (col <- 0 until field.width) yield {
      def empty(row: Int) = movedField.empty((row, col))
      (movedField.height-1 to 0 by -1) dropWhile empty filter empty map {(_, col)}
    }
    colHoles.flatten
  }

  lazy val holeDepth = {
    def depth(hole: Block): Int = {
      val (row, col) = hole
      (row until movedField.height) count {row => !movedField.empty((row, col))}
    }

    (holes map depth).sum
  }

  lazy val holeCount = holes.size

  lazy val lostGame: Boolean = {
    val top = movedField.height - 1
    blocksInRegion(top+4, 0, top+1, movedField.width - 1) ||
    blocksInRegion(top, spawnStartCol, top, spawnEndCol)
  }

  lazy val path = {
    def heuristic(start: Position, goal: Position): Double = {
      import math._

      val ((startX, startY), startAngle) = start
      val ((goalX, goalY), goalAngle) = goal
      val angleDiff = abs(AiBlockBattle.normalizeAngle(goalAngle - startAngle)) / 90
      val diffX = (goalX - startX).toDouble
      val diffY = (goalY - startY).toDouble

      diffX * diffX + diffY * diffY + angleDiff.toDouble
    }

    def getNeighbors(field: Field, piece: Piece)(position: Position): Set[Position] = {
      val ((row, col), angle) = position
      val allNeighbors = Set(((row-1, col), angle),
                             ((row, col-1), angle),
                             ((row, col+1), angle),
                             ((row, col), AiBlockBattle.normalizeAngle(angle - 90)),
                             ((row, col), AiBlockBattle.normalizeAngle(angle + 90)))

      allNeighbors filter {neighbor => field.moveValid(piece.getBlocksFromPosition(neighbor))}
    }

    val aStar = new AStar(heuristic, getNeighbors(field, piece)_)
    val goals = positions.toArray.sortBy(position => math.abs(position._2)).iterator
    val paths = goals map {aStar.getPath(start, _)} dropWhile {_.isEmpty}
    if (paths.hasNext)
      paths.next()
    else
      List.empty[Position]
  }

  lazy val points = {
    if (clearCount == 4)
      8
    else if (clearCount > 0)
      clearCount + combo
    else
      0
  }

  def <(other: Metric): Boolean = {
    def boolField(f: Metric => Boolean): Metric => Int = {
      x => if (f(x)) 1 else 0
    }

    def largerIsBetter(f: Metric => Int): Metric => Int = {
      x => -1 * f(x)
    }

    val priorities: Seq[Metric => Int] = Seq(
      boolField(_.path.isEmpty), // TODO:  Make general move valid
      boolField(_.lostGame),
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
