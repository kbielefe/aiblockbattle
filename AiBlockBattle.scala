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

object AiBlockBattle {
  type GameState = Map[String, String]
  type Field = Array[Array[Boolean]]
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
    val my_field = getField(state(my_bot + "/field"))
    val this_piece_type = state("game/this_piece_type")(0)
    val this_piece_position = state("game/this_piece_position") split ','
    val start = ((this_piece_position(1).toInt, this_piece_position(0).toInt), 0)
    val width = state("field_width").toInt
    val boundaries = getBoundaries(my_field)
    val piece = pieces(this_piece_type)
    val potentialPositions = piece.getPositionsFromBoundaries(boundaries).toSet
    val potentialBlocks = potentialPositions map {position => (position, piece.getBlocksFromPosition(position))}
    val groupedBlocks = potentialBlocks groupBy {_._2} mapValues {_ map {_._1}}
    val validMoves = groupedBlocks filter {group => moveValid(my_field, width)(group._1)}
    val goal = validMoves.head._2.head

    val aStar = new AStar(heuristic, getNeighbors(my_field)_)
    val path = aStar.getPath(start, goal)
    println(pathToMoves(path).mkString(","))
  }

  def moveValid(field: Field, width: Int)(move: Set[Block]): Boolean = {
    move forall {case (row, col) => col >= 0 && col < width && (row < 0 || field(row)(col))}
  }

  def getField(field: String): Field = {
    field split ';' map {_ split ',' map {cell => cell != "3" && cell != "4"}}
  }

  def getBoundaries(field: Field): IndexedSeq[Block] = {
    def isBoundary(block: Block): Boolean = {
      val (row, col) = block
      !field(row+1)(col) && field(row)(col)
    }

    val height = field.size - 1
    val width  = field(0).size

    val blocks = for (row <- 0 until height; col <- 0 until width) yield (row, col)
    val bottomBlocks = for (col <- 0 until width) yield (height, col)
    (blocks filter isBoundary) ++ (bottomBlocks filter {case (row, col) => field(row)(col)})
  }

  def printField(field: Field): Unit = {
    field map {_ mkString ""} foreach println
  }

  def printState(state: GameState): Unit = {
    def printSetting(setting: (String, String)): Unit = {
      val (a, b) = setting
      if (a endsWith "/field") {
        println(a + " -> ")
        printField(getField(b))
      } else {
        println(a + " -> " + b)
      }
    }

    state foreach printSetting
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
    val angleDiff = abs(normalizeAngle(goalAngle - startAngle))
    val diffX = (goalX - startX).toDouble
    val diffY = (goalY - startY).toDouble

    diffX * diffX + diffY * diffY + angleDiff.toDouble
  }

  def getNeighbors(field: Field)(position: Position): Set[Position] = {
    val ((x, y), angle) = position
    //TODO: filter valid moves for field
    Set(((x, y+1), angle),
        ((x-1, y), angle),
        ((x+1, y), angle),
        ((x, y), normalizeAngle(angle - 90)),
        ((x, y), normalizeAngle(angle + 90)))
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
