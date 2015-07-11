object AiBlockBattle {
  // Map from character to set of rotations
  // Map from rotation to receptor width and height
  //   either 4x0 3x0 2x0 1x0 2x2 3x1 2x1 
  // Map from rotation to map from receptor to offset
  //
  // Settings map from String to String
  // Drop moves
  // Slide moves

  type GameState = Map[String, String]

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
    val my_field = getField(state, my_bot)
  }

  def getField(state: GameState, bot: String): Array[Array[Int]] = {
    state(bot + "/field") split ';' map {_ split ',' map {_.toInt}}
  }

  def printField(field: Array[Array[Int]]): Unit = {
    field map {_ mkString ","} foreach println
  }

  def printState(state: GameState): Unit = {
    def printField(field: (String, String)): Unit = {
      val (a, b) = field
      if (a endsWith "/field") {
        println(a + " -> ")
        b split ';' map {"  " + _} foreach println
      } else {
        println(a + " -> " + b)
      }
    }

    state foreach printField
  }

  def main(args: Array[String]) {
    val lines = io.Source.stdin.getLines
    val state = lines.foldLeft(Map[String, String]())(processLine)
    printState(state)
  }
}
