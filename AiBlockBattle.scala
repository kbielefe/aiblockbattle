object AiBlockBattle {
  type GameState = Map[String, String]
  type Field = Array[Array[Int]]
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
    move forall {case (row, col) => row >= 0 && col >= 0 && col < width && isEmpty(field)((row, col))}
  }

  def getField(field: String): Field = {
     field split ';' map {_ split ',' map {_.toInt}}
  }

  def isEmpty(field: Field)(block: Block): Boolean = {
    val (row, col) = block
    val cell = field(row)(col)
    cell == 0 || cell == 1
  }

  def getBoundaries(field: Field): IndexedSeq[Block] = {
    def isBoundary(block: Block): Boolean = {
      val (row, col) = block
      val bottom = (row+1, col)
      !isEmpty(field)(bottom) && isEmpty(field)(block)
    }

    val height = field.size - 1
    val width  = field(0).size

    val blocks = for (row <- 0 until height; col <- 0 until width) yield (row, col)
    val bottomBlocks = for (col <- 0 until width) yield (height, col)
    (blocks filter isBoundary) ++ (bottomBlocks filter isEmpty(field))
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
