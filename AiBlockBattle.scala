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
    'Z' -> "XX  XX   ") map {case (name, string) => (name, new Piece(string, name))}

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
    val my_field = Field(state(my_bot + "/field"))
    val combo = state(my_bot + "/combo").toInt
    val this_piece_type = state("game/this_piece_type")(0)
    val this_piece_position = state("game/this_piece_position") split ','
    val start = ((this_piece_position(1).toInt, this_piece_position(0).toInt), 0)
    val boundaries = my_field.getBoundaries
    val piece = pieces(this_piece_type)
    val potentialPositions = piece.getPositionsFromBoundaries(boundaries).toSet
    val potentialBlocks = potentialPositions map {position => (position, piece.getBlocksFromPosition(position))}
    val groupedBlocks = potentialBlocks groupBy {_._2} mapValues {_ map {_._1}}
    val validMoves = groupedBlocks filter {block => my_field.moveValid(block._1)}
    val metrics = validMoves map {case (blocks, positions) => new Metric(blocks, positions, my_field, piece, start, combo)}
    val sortedMetrics = metrics.toArray.sortWith((left, right) => right < left)

    sortedMetrics foreach Console.err.println

    val path = sortedMetrics.dropWhile(_.path.size == 0)
    if (path.isEmpty)
      println("no_moves")
    else {
      Console.err.println(path.head.positions.head)
      println(pathToMoves(path.head.path).mkString(","))
    }
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
