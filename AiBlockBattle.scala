object AiBlockBattle {
  type GameState = Map[String, String]
  type Block = (Int, Int)
  type Position = (Block, Int) // Origin, angle

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
    val field = Field(state(my_bot + "/field"))
    val combo = state(my_bot + "/combo").toInt
    val points = state(my_bot + "/row_points").toInt
    val pieceName = state("game/this_piece_type")(0)
    val nextPiece = state("game/next_piece_type")

    val tree = new BlockTree(Node(field, pieceName, nextPiece, points, combo), true)

    val minimax = new Minimax[Position, Node, Int]()
    minimax.scoreTree(tree, 1)

    //val start = ((field.height - this_piece_position(1).toInt - piece.width, this_piece_position(0).toInt), 0)
    val path = List.empty[Position]

    if (path.isEmpty)
      println("no_moves")
    else {
      println(pathToMoves(path).mkString(","))
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
    val state = lines.foldLeft(Map.empty[String, String])(processLine)
  }
}
