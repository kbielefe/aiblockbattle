case class GameState(map: Map[String, String], tree: Tree[((Int, Int), Int), Node, Metric], depth: Int, first: Boolean)

object AiBlockBattle {
  import scala.annotation.tailrec
  type Block = (Int, Int)
  type Position = (Block, Int) // Origin, angle

  def processLine(state: GameState, line: String): GameState = {
    val fields = line split ' '

    fields(0) match {
      case "settings" => GameState(state.map + (fields(1) -> fields(2)), state.tree, state.depth, state.first)
      case "update"   => GameState(state.map + (fields(1) + "/" + fields(2) -> fields(3)), state.tree, state.depth, state.first)
      //case "action"   => if (state.first) outputFirstMove(state, fields(2).toLong) else outputMove(state, fields(2).toLong)
      case "action"   => outputFirstMove(state, fields(2).toLong)
      case _          => state
    }
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
    val allNeighbors = Set(((row-1, col), angle),
      ((row, col-1), angle),
      ((row, col+1), angle),
      ((row, col), normalizeAngle(angle - 90)),
      ((row, col), normalizeAngle(angle + 90)))

    allNeighbors filter {neighbor => field.moveValid(piece.getBlocksFromPosition(neighbor))}
  }

  val minimax = new Minimax[Position, Node, Metric]()

  def outputFirstMove(state: GameState, time: Long): GameState = {
    val startTime = System.currentTimeMillis()
    val my_bot = state.map("your_bot")
    val field = Field(state.map(my_bot + "/field"))
    val combo = state.map(my_bot + "/combo").toInt
    val points = state.map(my_bot + "/row_points").toInt
    val pieceName = state.map("game/this_piece_type")(0)
    val nextPiece = state.map("game/next_piece_type")
    val piece = Piece(pieceName)
    val this_piece_position = state.map("game/this_piece_position") split ","

    val start = ((field.height - this_piece_position(1).toInt - piece.width, this_piece_position(0).toInt), 0)

    val tree = new BlockTree(Node(1, field, ((-1, -1), -1), pieceName, nextPiece, points, combo), true)

    val (move, depth) = iterativeDeepening(tree, 1, startTime + 470)

    val fastPath = new FastPath(heuristic, getNeighbors(field, piece)_)
    val path = fastPath.getPath(start, move)

    if (path.isEmpty)
      println("no_moves")
    else {
      println(pathToMoves(path).mkString(","))
    }
    Console.err.println(depth)
    GameState(state.map, tree.getChildren.head._2.getChildren.head._2, depth, false)
  }

  def nextPiecePrune(piece: Char, round: Int)(node: (Position, Tree[Position, Node, Metric])): Boolean = {
    val state = node._2.state
    !(state.round == round + 1 && state.piece != piece)
  }

  def outputMove(state: GameState, time: Long): GameState = {
    val startTime = System.currentTimeMillis()
    val my_bot = state.map("your_bot")
    val field = Field(state.map(my_bot + "/field"))
    val round = state.map("game/round").toInt
    val pieceName = state.map("game/this_piece_type")(0)
    //val nextPiece = state.map("game/next_piece_type")(0)
    val nextPiece = state.map("game/next_piece_type")
    val piece = Piece(pieceName)
    val this_piece_position = state.map("game/this_piece_position") split ","
    val start = ((field.height - this_piece_position(1).toInt - piece.width, this_piece_position(0).toInt), 0)
    val combo = state.map(my_bot + "/combo").toInt

    //val tree = state.tree
    //tree.prune(nextPiecePrune(nextPiece, round)_)
    // update for height
    val tree = new BlockTree(Node(1, field, ((-1, -1), -1), pieceName, nextPiece, 0, combo), true)

    val (move, depth) = iterativeDeepening(tree, math.max(1, state.depth - 2), startTime + 470)

    val fastPath = new FastPath(heuristic, getNeighbors(field, piece)_)
    val path = fastPath.getPath(start, move)

    if (path.isEmpty)
      println("no_moves")
    else {
      println(pathToMoves(path).mkString(","))
    }
    Console.err.println(depth)
    //Console.err.println(System.currentTimeMillis() - startTime)
    GameState(state.map, tree.getChildren.head._2.getChildren.head._2, depth, false)
  }

  @tailrec
  def iterativeDeepening(tree: Tree[Position, Node, Metric], depth: Int, deadline: Long, completedMove: Position = ((-1, -1), -1)): (Position, Int) = {
    val move = minimax.run(tree, depth, deadline)
    if (System.currentTimeMillis() > deadline) {
      if (depth >= 5)
        (completedMove, depth)
      else
        (move, depth)
    } else {
      iterativeDeepening(tree, depth + 2, deadline, move)
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
    val initialField = new Field(Set.empty[(Int, Int)], 0, 0)
    val initialTree = new BlockTree(Node(1, initialField, ((-1, -1), -1), 'I', "", 0, 0), true)
    val initialGameState = GameState(Map.empty[String, String], initialTree, 1, true)

    val state = lines.foldLeft(initialGameState)(processLine)
  }
}
