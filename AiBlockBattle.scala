class aStar(start: (Int, Int), field: Array[Array[Boolean]]) {
  import scala.annotation.tailrec
  import math.sqrt
  type Cell = (Int, Int)

  def heuristic(goal: Cell)(position: Cell): Double = {
    val (x, y) = position
    val (goalX, goalY) = goal
    val dx = (x - goalX).toDouble
    val dy = (y - goalY).toDouble

    sqrt(dx * dx + dy * dy)
  }

  def reconstructPath(start: Cell, goal: Cell, cameFrom: Map[Cell, Cell]): Cell = {
    @tailrec
    def helper(current: Cell): Cell = {
      if (cameFrom(current) == start)
        return current
      helper(cameFrom(current))
    }

    helper(goal)
  }

  def aStar(goal: Cell, obstacles: Set[Cell]) : Cell = {
    @tailrec
    def helper(closed: Set[Cell], open: Set[Cell], cameFrom: Map[Cell, Cell],
      g: Map[Cell, Double], f: Map[Cell, Double]): Cell = {

      val current = open minBy {f(_)}
      if (current == goal)
        return reconstructPath(start, goal, cameFrom)
      val tentativeG = g(current) + 1.0

      val neighbors = getNeighbors(current) -- closed -- obstacles
      val notOpenNeighbors = neighbors -- open
      val betterNeighbors = (neighbors & open) filter {tentativeG < g(_)}
      val newNeighbors = notOpenNeighbors ++ betterNeighbors

      val newCameFrom = cameFrom ++ (newNeighbors map {(_, current)})
      val newG = g ++ (newNeighbors map {(_, tentativeG)})
      val newF = f ++ (newNeighbors map {neighbor => (neighbor, tentativeG + heuristic(goal)(neighbor))})

      val newClosed = closed + current
      val newOpen = open ++ newNeighbors - current

      helper(newClosed, newOpen, newCameFrom, newG, newF)
    }

    val closed = Set[Cell]()
    val open = Set(start)
    val cameFrom = Map[Cell, Cell]()
    val g = Map[Cell, Double](start -> 0.0)
    val f = Map[Cell, Double](start -> heuristic(goal)(start))
    helper(closed, open, cameFrom, g, f)
  }

  def getNeighbors(position: Cell): Set[Cell] = {
    val (x, y) = position
    Set((x-1, y-1), (x, y-1), (x+1, y-1), (x-1, y), (x+1, y), (x-1, y+1), (x, y+1), (x+1, y+1))
  }
}

object AiBlockBattle {
  type GameState = Map[String, String]
  type Field = Array[Array[Boolean]]
  type Block = (Int, Int)

  val pieces = Map(
    'I' -> "    XXXX        ",
    'J' -> "X  XXX   ",
    'L' -> "  XXXX   ",
    'O' -> "XXXX",
    'S' -> "XX  XX   ",
    'T' -> " X XXX   ",
    'Z' -> " XXXX    ")

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
    val width = state("field_width").toInt
    val boundaries = getBoundaries(my_field)
    val potentialMoves = for (i <- pieceSets(this_piece_type); j <- boundaries) yield getMoves(i, j)
    val validMoves = potentialMoves filter moveValid(my_field, width)_
  }

  def getMoves(piece: Set[Block], boundary: Block): Set[Block] = {
    val (row, col) = boundary
    val result = piece map {case (pieceRow, pieceCol) => (pieceRow + row, pieceCol + col)}
    result
  }

  def moveValid(field: Field, width: Int)(move: Set[Block]): Boolean = {
    move forall {case (row, col) => row >= 0 && col >= 0 && col < width && !field(row)(col)}
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
    (blocks filter isBoundary) ++ (bottomBlocks filter {case (row, col) => !field(row)(col)})
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

  def rotateRight(piece: IndexedSeq[Block], width: Int): IndexedSeq[Block] = {
     piece map {case (row, col) => (col, width - row - 1)}
  }

  def rotateLeft(piece: IndexedSeq[Block], width: Int): IndexedSeq[Block] = {
     piece map {case (row, col) => (width - col - 1, row)}
  }

  def getPieceBoundaries(piece: IndexedSeq[Block]): Iterable[Block] = {
    val grouped = piece groupBy {_._2}
    grouped.values map {_ maxBy {_._1}}
  }

  def normalizePiece(piece: IndexedSeq[Block], boundary: Block): Set[Block] = {
    val (row, col) = boundary
    val result = piece map {case (pieceRow, pieceCol) => (pieceRow - row, pieceCol - col)}
    result.toSet
  }

  def getNormalizedPieces(piece: IndexedSeq[Block]): Set[Set[Block]] = {
    val boundaries = getPieceBoundaries(piece)
    val result = boundaries map {normalizePiece(piece, _)}
    result.toSet
  }

  def getPieceSet(piece: String): Set[Set[Block]] = {
    val pieceBool = piece map {_ == 'X'}
    val indexes = pieceBool.zipWithIndex filter {_._1} map {_._2}
    val width = math.floor(math.sqrt(pieceBool.size)).toInt
    val result = indexes map {index => (index / width, index % width)}
    val rotations = Set(result, rotateRight(result, width), rotateLeft(result, width), rotateLeft(rotateLeft(result, width), width))
    rotations flatMap getNormalizedPieces
  }

  val pieceSets = pieces mapValues getPieceSet

  def main(args: Array[String]) {
    val lines = io.Source.stdin.getLines
    val state = lines.foldLeft(Map[String, String]())(processLine)
  }
}
