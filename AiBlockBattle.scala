class AStar[Position](
    heuristic: (Position, Position) => Double,
    getNeighbors: Position => Set[Position]) {
  import scala.annotation.tailrec

  def getPath(start: Position, goal: Position) : List[Position] = {
    @tailrec
    def helper(closed: Set[Position], open: Set[Position], cameFrom: Map[Position, Position],
      g: Map[Position, Double], f: Map[Position, Double]): List[Position] = {

      if (open.isEmpty)
        return List[Position]()

      val current = open minBy {f(_)}
      if (current == goal)
        return reconstructPath(start, goal, cameFrom)

      val tentativeG = g(current) + 1.0

      val neighbors = getNeighbors(current) -- closed
      val notOpenNeighbors = neighbors -- open
      val betterNeighbors = (neighbors & open) filter {tentativeG < g(_)}
      val newNeighbors = notOpenNeighbors ++ betterNeighbors

      val newCameFrom = cameFrom ++ (newNeighbors map {(_, current)})
      val newG = g ++ (newNeighbors map {(_, tentativeG)})
      val newF = f ++ (newNeighbors map {neighbor => (neighbor, tentativeG + heuristic(neighbor, goal))})

      val newClosed = closed + current
      val newOpen = open ++ newNeighbors - current

      helper(newClosed, newOpen, newCameFrom, newG, newF)
    }

    val closed = Set[Position]()
    val open = Set(start)
    val cameFrom = Map[Position, Position]()
    val g = Map[Position, Double](start -> 0.0)
    val f = Map[Position, Double](start -> heuristic(start, goal))
    helper(closed, open, cameFrom, g, f)
  }

  private def reconstructPath(start: Position, goal: Position, cameFrom: Map[Position, Position]): List[Position] = {
    @tailrec
    def helper(current: Position, result: List[Position]): List[Position] = {
      if (current == start)
        return start :: result
      helper(cameFrom(current), current :: result)
    }

    helper(goal, List[Position]())
  }
}

class Piece(string: String) {
  type Block = (Int, Int)
  type Position = (Block, Int)

  val width = math.floor(math.sqrt(string.size)).toInt

  val blocks = {
    val indexes = string.zipWithIndex filter {_._1 == 'X'} map {_._2}
    val indexSeq = indexes map {index => (index / width, index % width)}
    indexSeq.toSet
  }

  def getPositionsFromBoundaries(boundaries: Iterable[Block]): Iterable[Position] = {
    boundaries flatMap getPositionsFromBoundary
  }

  def getPositionsFromBoundary(boundary: Block): Set[Position] = {
    val angles = Set(0, -90, 90, 180)
    val offset = angles flatMap getBoundariesFromAngle
    offset map {case ((row, col), angle) => ((boundary._1 - row, boundary._2 - col), angle)}
  }

  def getBlocksFromAngle(angle: Int): Set[Block] = angle match {
    case -90 => blocks map rotateLeft
    case  90 => blocks map rotateRight
    case 180 => blocks map flip
    case   _ => blocks
  }

  def getBlocksFromPosition(position: Position): Set[Block] = {
    val ((row, col), angle) = position
    val rotated = getBlocksFromAngle(angle)
    rotated map {case (blockX, blockY) => (row + blockX, col + blockY)}
  }

  private def getBoundariesFromAngle(angle: Int): Iterable[Position] = {
    val grouped = getBlocksFromAngle(angle) groupBy {_._2}
    grouped.values map {col => (col maxBy {_._1}, angle)}
  }

  private def rotateRight(block: Block): Block = (block._2, width - block._1 - 1)

  private def rotateLeft(block: Block): Block = (width - block._2 - 1, block._1)

  private def flip(block: Block): Block = (width - block._1 - 1, width - block._2 - 1)
}

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

  def moveValid(move: Set[Block]): Boolean = {
    move forall isEmpty
  }
}

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

  val (clearCount, cleared) = (field.height - 1 to 0 by -1).foldLeft((0, Set[Block]()))(removeRow)

  def isEmpty(block: Block): Boolean = {
    !(cleared contains block)
  }

  def lostGame: Boolean = cleared exists {_._1 < 0}

}

case class Metric(blocks: Set[(Int, Int)], positions: Set[((Int, Int), Int)], field: Field, piece: Piece, start: ((Int, Int), Int), combo: Int) {
  type Block = (Int, Int)
  type Position = (Block, Int) // Origin, angle

  private lazy val movedField = new MovedField(field, blocks)

  lazy val blockHeight = blocks.toList.map(_._1).sum

  lazy val holeCount = {
    val colHoles = for (col <- 0 until field.width) yield {
      def empty(row: Int) = movedField.isEmpty((row, col))
      (0 until field.height) dropWhile empty count empty
    }
    colHoles.sum
  }

  lazy val lostGame = movedField.lostGame

  lazy val path = {
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

  private def heuristic(start: Position, goal: Position): Double = {
    import math._

    val ((startX, startY), startAngle) = start
    val ((goalX, goalY), goalAngle) = goal
    val angleDiff = abs(AiBlockBattle.normalizeAngle(goalAngle - startAngle)) / 90
    val diffX = (goalX - startX).toDouble
    val diffY = (goalY - startY).toDouble

    diffX * diffX + diffY * diffY + angleDiff.toDouble
  }

  private def getNeighbors(field: Field, piece: Piece)(position: Position): Set[Position] = {
    val ((row, col), angle) = position
    val allNeighbors = Set(((row+1, col), angle),
                           ((row, col-1), angle),
                           ((row, col+1), angle),
                           ((row, col), AiBlockBattle.normalizeAngle(angle - 90)),
                           ((row, col), AiBlockBattle.normalizeAngle(angle + 90)))

    allNeighbors filter {neighbor => field.moveValid(piece.getBlocksFromPosition(neighbor))}
  }
}

object AiBlockBattle {
  type GameState = Map[String, String]
  type Block = (Int, Int)
  type Position = (Block, Int) // Origin, angle

  val pieces = Map(
    'I' -> "    XXXX        ",
    'J' -> "X  XXX   ",
    'L' -> "  XXXX   ",
    'O' -> "XXXX",
    'S' -> " XXXX    ",
    'T' -> " X XXX   ",
    'Z' -> "XX  XX   ") mapValues {new Piece(_)}

  def processLine(state: GameState, line: String): GameState = {
    val fields = line split ' '

    fields(0) match {
      case "settings" => state + (fields(1) -> fields(2))
      case "update"   => state + (fields(1) + "/" + fields(2) -> fields(3))
      case "action"   => outputMove(state, fields(2).toInt); state
      case _          => state
    }
  }

  def outputMove(state: GameState, time: Int): Unit = {
    val my_bot = state("your_bot")
    val my_field = new Field(state(my_bot + "/field"))
    val combo = state(my_bot + "/combo").toInt
    val this_piece_type = state("game/this_piece_type")(0)
    val this_piece_position = state("game/this_piece_position") split ','
    val start = ((this_piece_position(1).toInt, this_piece_position(0).toInt), 0)
    val boundaries = getBoundaries(my_field)
    val piece = pieces(this_piece_type)
    val potentialPositions = piece.getPositionsFromBoundaries(boundaries).toSet
    val potentialBlocks = potentialPositions map {position => (position, piece.getBlocksFromPosition(position))}
    val groupedBlocks = potentialBlocks groupBy {_._2} mapValues {_ map {_._1}}
    val validMoves = groupedBlocks filter {block => my_field.moveValid(block._1)}
    val metrics = validMoves map {case (blocks, positions) => new Metric(blocks, positions, my_field, piece, start, combo)}
    val sortedMetrics = metrics.toArray.filterNot(_.lostGame).sortBy(-1 * _.blockHeight).sortBy(_.holeCount).sortBy(-1 * _.points)
    val path = sortedMetrics.dropWhile(_.path.size == 0)
    if (path.isEmpty)
      println("no_moves")
    else
      println(pathToMoves(path.head.path).mkString(","))
  }

  def getBoundaries(field: Field): IndexedSeq[Block] = {
    def isBoundary(block: Block): Boolean = {
      val (row, col) = block
      val below = (row+1, col)
      !field.isEmpty(below) && field.isEmpty(block)
    }

    val blocks = for (row <- 0 until field.height; col <- 0 until field.width) yield (row, col)
    blocks filter isBoundary
  }

  def normalizeAngle(angle: Int): Int = {
    if (angle > 180)
      angle - 360
    else if (angle <= -180)
      angle + 360
    else
      angle
  }

  def pathToMoves(path: List[Position]): Iterator[String] = {
    def pairToMove(pair: List[Position]): String = {
      val List(first, second) = pair
      val ((firstRow, firstCol), firstAngle) = first
      val ((secondRow, secondCol), secondAngle) = second

      if (firstRow != secondRow) {
        "down"
      } else if (firstCol < secondCol) {
        "right"
      } else if (firstCol > secondCol) {
        "left"
      } else if (normalizeAngle(firstAngle - secondAngle) == -90) {
        "turnright"
      } else if (normalizeAngle(firstAngle - secondAngle) == 90) {
        "turnleft"
      } else {
        "confused"
      }
    }

    if (path.size < 2)
      return Iterator("no_moves")

    path.sliding(2) map pairToMove
  }

  def main(args: Array[String]) {
    val lines = io.Source.stdin.getLines
    val state = lines.foldLeft(Map[String, String]())(processLine)
  }
}
