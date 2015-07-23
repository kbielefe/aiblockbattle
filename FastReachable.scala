// Quickly determine if a start position can reach the goal position
class FastReachable[Position](
    heuristic: (Position, Position) => Double,
    getNeighbors: Position => Set[Position],
    valid: Position => Boolean) {
  import scala.annotation.tailrec

  def reachable(start: Position, goal: Position) : Boolean = {
    @tailrec
    def helper(closed: Set[Position], open: Set[Position]): Boolean = {

      if (open.isEmpty)
        return false

      //TODO:  Make a priority queue
      val current = open minBy {heuristic(_, goal)}

      if (current == goal)
        return true

      if (valid(current))
        helper(closed + current, open ++ (getNeighbors(current) -- closed) - current)
      else
        helper(closed + current, open - current)
    }

    val closed = Set.empty[Position]
    val open = Set(start)
    helper(closed, open)
  }
}
