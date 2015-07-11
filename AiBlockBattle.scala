object AiBlockBattle {
  // Map from character to set of rotations
  // Map from rotation to receptor width and height
  //   either 4x0 3x0 2x0 1x0 2x2 3x1 2x1 
  // Map from rotation to map from receptor to offset
  //
  // Settings map from String to String

  type GameState = Map[String, String]

  def processLine(state: GameState, line: String): GameState = {
    val fields = line split ' '

    fields(0) match {
      case "settings" => state + (fields(1) -> fields(2))
      case "update"   => state + (fields(1) + "/" + fields(2) -> fields(3))
      case "action"   => outputMove(state); state
      case _          => state
    }
  }

  def outputMove(state: GameState): Unit = {
  }

  def main(args: Array[String]) {
    val lines = io.Source.stdin.getLines
    val state = lines.foldLeft(Map[String, String]())(processLine)
    state foreach println
  }
}
