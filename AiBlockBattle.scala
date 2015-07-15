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

  def moveValid(move: Set[Block]): Boolean = {
    move forall isEmpty
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
    val this_piece_type = state("game/this_piece_type")(0)
    val this_piece_position = state("game/this_piece_position") split ','
    val start = ((this_piece_position(1).toInt, this_piece_position(0).toInt), 0)
    val boundaries = getBoundaries(my_field)
    val piece = pieces(this_piece_type)
    val potentialPositions = piece.getPositionsFromBoundaries(boundaries).toSet
    val potentialBlocks = potentialPositions map {position => (position, piece.getBlocksFromPosition(position))}
    val groupedBlocks = potentialBlocks groupBy {_._2} mapValues {_ map {_._1}}
    val validMoves = groupedBlocks filter {block => my_field.moveValid(block._1)}
    val goal = validMoves.maxBy(move => move._1.toList.map(_._1).sum)._2.head

    val aStar = new AStar(heuristic, getNeighbors(my_field, piece)_)
    val path = aStar.getPath(start, goal)
    println(pathToMoves(path).mkString(","))
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

  def heuristic(start: Position, goal: Position): Double = {
    import math._

    val ((startX, startY), startAngle) = start
    val ((goalX, goalY), goalAngle) = goal
    val angleDiff = abs(normalizeAngle(goalAngle - startAngle)) / 90
    val diffX = (goalX - startX).toDouble
    val diffY = (goalY - startY).toDouble

    diffX * diffX + diffY * diffY + angleDiff.toDouble
  }

  def getNeighbors(field: Field, piece: Piece)(position: Position): Set[Position] = {
    val ((row, col), angle) = position
    val allNeighbors = Set(((row+1, col), angle),
                           ((row, col-1), angle),
                           ((row, col+1), angle),
                           ((row, col), normalizeAngle(angle - 90)),
                           ((row, col), normalizeAngle(angle + 90)))

    allNeighbors filter {neighbor => field.moveValid(piece.getBlocksFromPosition(neighbor))}
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
