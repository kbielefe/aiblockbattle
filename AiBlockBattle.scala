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
  type Field = Array[Array[Int]]
  type Block = (Int, Int)

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
    val this_piece_type = state("game/this_piece_type")
    val boundaries = getBoundaries(my_field)
    println(boundaries)
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

  def main(args: Array[String]) {
    val lines = io.Source.stdin.getLines
    val state = lines.foldLeft(Map[String, String]())(processLine)
  }
}
