abstract class Node {
  def heuristic: BlockMinimax.Infinite
  def getChildren: List[Node]

  val pieces = Map(
    'I' -> "    XXXX        ",
    'J' -> "X  XXX   ",
    'L' -> "  XXXX   ",
    'O' -> "XXXX",
    'S' -> " XXXX    ",
    'T' -> " X XXX   ",
    'Z' -> "XX  XX   ") map {case (name, string) => (name, new Piece(string, name))}
}

class RootNode(state: Map[String, String]) extends Node {
  val my_bot = state("your_bot")
  val field = Field(state(my_bot + "/field"))
  val combo = state(my_bot + "/combo").toInt
  val this_piece_type = state("game/this_piece_type")(0)
  val next_piece_type = state("game/next_piece_type")(0)
  val this_piece_position = state("game/this_piece_position") split ','
  val piece = pieces(this_piece_type)
  val next_piece = pieces(next_piece_type)
  val start = ((field.height - this_piece_position(1).toInt - piece.width, this_piece_position(0).toInt), 0)

  def getChildren: List[Node] = {
    val boundaries = field.getBoundaries
    val potentialPositions = piece.getPositionsFromBoundaries(boundaries).toSet
    val potentialBlocks = potentialPositions map {position => (position, piece.getBlocksFromPosition(position))}
    val groupedBlocks = potentialBlocks groupBy {_._2} mapValues {_ map {_._1}}
    val validMoves = groupedBlocks filter {block => field.moveValid(block._1)}
    val metrics = validMoves map {case (blocks, positions) => new Metric(blocks, positions, field, piece, start, combo, List.empty[List[((Int, Int), Int)]])}
    val sortedMetrics = metrics.toArray.sortWith((left, right) => right < left)
    //sortedMetrics foreach Console.err.println
    val sortedNodes = sortedMetrics map {new FirstLevelNode(_, next_piece)}
    sortedNodes.toList
  }

  def heuristic: BlockMinimax.Infinite = BlockMinimax.Infinity

}

class FirstLevelNode(metric: Metric, next_piece: Piece) extends Node {
  def heuristic: BlockMinimax.Infinite = BlockMinimax.Finite(metric)
  def getChildren: List[Node] = List(new SecondLevelNode(metric, next_piece))
}

class SecondLevelNode(metric: Metric, piece: Piece) extends Node {
  def heuristic: BlockMinimax.Infinite = BlockMinimax.Infinity
  def getChildren: List[Node] = {
    val field = metric.movedField
    val start = piece.getExpectedSpawnPosition(field)
    val combo = metric.newCombo
    val boundaries = field.getBoundaries
    val potentialPositions = piece.getPositionsFromBoundaries(boundaries).toSet
    val potentialBlocks = potentialPositions map {position => (position, piece.getBlocksFromPosition(position))}
    val groupedBlocks = potentialBlocks groupBy {_._2} mapValues {_ map {_._1}}
    val validMoves = groupedBlocks filter {block => field.moveValid(block._1)}
    val metrics = validMoves map {case (blocks, positions) => new Metric(blocks, positions, field, piece, start, combo, metric.path :: metric.parentPaths)}
    val sortedMetrics = metrics.toArray.sortWith((left, right) => right < left)
    //sortedMetrics foreach Console.err.println
    val sortedNodes = sortedMetrics map {new ThirdLevelNode(_)}
    sortedNodes.toList
  }
}

class ThirdLevelNode(metric: Metric) extends Node {
  def heuristic: BlockMinimax.Infinite = BlockMinimax.Finite(metric)
  def getChildren: List[Node] = List.empty[Node]
}

object BlockMinimax extends Minimax[Node, Metric] {
  def lt(left: Metric, right: Metric): Boolean = {
    left < right
  }

  def terminal(node: Node): Boolean = false
  def heuristic(node: Node): Infinite = node.heuristic
  def getChildren(node: Node): List[Node] = node.getChildren
}
