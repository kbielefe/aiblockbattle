case class Metric(blocks: Set[(Int, Int)], positions: Set[((Int, Int), Int)], field: Field, piece: Piece, start: ((Int, Int), Int), combo: Int) {
  type Block = (Int, Int)
  type Position = (Block, Int) // Origin, angle

  override
  def toString: String = {
    blockHeight + " " + holeCount + " " + lostGame + " " + chimneyCount + " " + holeDepth + " " + positions.head
  }

  private lazy val movedField = new MovedField(field, blocks)

  lazy val blockHeight = blocks.toList.map(_._1).sum

  lazy val distanceFromPreferredSide = piece.getDistanceFromPreferredSide(positions.head, field.width)

  lazy val chimneyCount = {
    val blocks = for (row <- 0 until field.height; col <- 0 until field.width) yield (row, col)
    blocks count {case (row, col) => movedField.isEmpty((row, col)) &&
      movedField.isEmpty((row-1, col)) && !movedField.isEmpty((row-1,col-1)) && !movedField.isEmpty((row-1,col+1))
    }
  }

  lazy val endGameBlocks = {
    val blocks = for (row <- 0 to 2; col <- 0 until field.width) yield (row, col)
    blocks count {!movedField.isEmpty(_)}
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
    (before count {!movedField.isEmpty(_)}) +
      (underSpawn count {!movedField.isEmpty(_)}) +
      (after count {!movedField.isEmpty(_)})
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
    val count = movedField.clearCount 
    if (count == 4)
      8
    else if (count > 0)
      count + combo
    else
      0
  }
}
