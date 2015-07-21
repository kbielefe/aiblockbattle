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
    val root = new RootNode(state)
    val path = BlockMinimax.search(root, 1).path

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
