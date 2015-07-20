case class Metric(
  blocks: Set[(Int, Int)],
  positions: Set[((Int, Int), Int)],
  field: Field,
  piece: Piece,
  start: ((Int, Int), Int),
  combo: Int) {

  type Block = (Int, Int)
  type Position = (Block, Int) // Origin, angle

  override
  def toString: String = {
    blockHeight + " " + holeCount + " " + lostGame + " " + chimneyCount + " " + holeDepth + " " + positions.head
  }

  private lazy val (movedField, clearCount) = field + blocks

  lazy val blockHeight = blocks.toList.map(_._1).sum

  lazy val distanceFromPreferredSide = piece.getDistanceFromPreferredSide(positions.head, field.width)

  lazy val chimneyCount = {
    val blocks = for (row <- 0 until field.height; col <- 0 until field.width) yield (row, col)
    blocks count {case (row, col) => movedField.empty((row, col)) &&
      movedField.empty((row-1, col)) && !movedField.empty((row-1,col-1)) && !movedField.empty((row-1,col+1))
    }
  }

  private val spawnStartCol = (field.width - 4) / 2
  private val spawnEndCol = spawnStartCol + 3

  private lazy val blocksInSpawn = {
    val blocks = for (col <- spawnStartCol to spawnEndCol) yield (0, col)
    blocks count {!movedField.isEmpty(_)}
  }

  def loseInX(x: Int): Int = {
    val before = for (col <- 0 until spawnStartCol) yield (x-1, col)
    val underSpawn = for (col <- spawnStartCol to spawnEndCol) yield (x, col)
    val after = for (col <- spawnEndCol+1 until field.width) yield (x-1, col)
    val anyExists = (before exists {!movedField.isEmpty(_)}) || 
      (underSpawn exists {!movedField.isEmpty(_)}) ||
      (after exists {!movedField.isEmpty(_)})
    if (anyExists) 1 else 0
  }

  private lazy val holes = {
    val colHoles = for (col <- 0 until field.width) yield {
      def empty(row: Int) = movedField.isEmpty((row, col))
      (0 until field.height) dropWhile empty filter empty map {(_, col)}
    }
    colHoles.flatten
  }

  lazy val holeDepth = {
    def depth(hole: Block): Int = {
      val (row, col) = hole
      (row-1 to 0 by -1) count {row => !movedField.isEmpty((row, col))}
    }

    (holes map depth).sum
  }

  lazy val holeCount = holes.size

  lazy val lostGame = movedField.lostGame || blocksInSpawn > 0

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
      val allNeighbors = Set(((row+1, col), angle),
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
      List[Position]()
  }

  lazy val points = {
    if (clearCount == 4)
      8
    else if (clearCount > 0)
      count + combo
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
      boolField(_.lostGame),
      _.loseInX(1),
      largerIsBetter(_.points),
      _.loseInX(2),
      _.loseInX(3),
      _.holeCount,
      _.chimneyCount,
      largerIsBetter(_.blockHeight),
      _.holeDepth,
      _.distanceFromPreferredSide.toInt)

    val unequal = priorities dropWhile {f => f(this) == f(other)}
    if (unequal.isEmpty)
      return false

    val f = unequal.head
    f(this) > f(other) // Inverted because for most, larger is worse
  }
}
