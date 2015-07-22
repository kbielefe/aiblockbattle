// Like AStar, but finds fastest path to calculate,
// not necessarily shortest path
class FastPath[Position](
    heuristic: (Position, Position) => Double,
    getNeighbors: Position => Set[Position]) {
  import scala.annotation.tailrec

  def getPath(start: Position, goal: Position) : List[Position] = {
    @tailrec
    def helper(closed: Set[Position], open: Set[Position], cameFrom: Map[Position, Position],
      f: Map[Position, Double]): List[Position] = {

      if (open.isEmpty)
        return List.empty[Position]

      val current = open minBy {f(_)}
      if (current == goal)
        return reconstructPath(start, goal, cameFrom)

      val neighbors = getNeighbors(current) -- closed -- open

      val newCameFrom = cameFrom ++ (neighbors map {(_, current)})
      val newF = f ++ (neighbors map {neighbor => (neighbor, heuristic(neighbor, goal))})

      val newClosed = closed + current
      val newOpen = open ++ neighbors - current

      helper(newClosed, newOpen, newCameFrom, newF)
    }

    val closed = Set[Position]()
    val open = Set(start)
    val cameFrom = Map[Position, Position]()
    val f = Map[Position, Double](start -> heuristic(start, goal))
    helper(closed, open, cameFrom, f)
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
