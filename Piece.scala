class Piece(string: String, name: Char) {
  type Block = (Int, Int)
  type Position = (Block, Int)

  val width = math.floor(math.sqrt(string.size)).toInt

  private val preferredSide = Map(
    ('J',   0) -> 0.0,
    ('J', -90) -> 1.0,
    ('J',  90) -> 0.0,
    ('J', 180) -> 1.0,
    ('L',   0) -> 1.0,
    ('L', -90) -> 1.0,
    ('L',  90) -> 0.0,
    ('L', 180) -> 0.0,
    ('S',   0) -> 0.0,
    ('S', -90) -> 1.0,
    ('S',  90) -> 1.0,
    ('S', 180) -> 0.0,
    ('T',   0) -> 0.4,
    ('T', -90) -> 1.0,
    ('T',  90) -> 0.0,
    ('T', 180) -> 0.4,
    ('Z',   0) -> 1.0,
    ('Z', -90) -> 0.0,
    ('Z',  90) -> 0.0,
    ('Z', 180) -> 1.0)

  def getDistanceFromPreferredSide(position: Position, width: Int): Double = {
    val ((row, col), angle) = position
    def distance(side: Double): Double = math.abs(col.toDouble - width.toDouble * side)
    if (preferredSide contains (name, angle)) {
      distance(preferredSide((name, angle)))
    } else {
      math.min(distance(1.0), distance(0.0))
    }
  }

  val blocks = {
    val indexes = string.zipWithIndex filter {_._1 == 'X'} map {_._2}
    val indexSeq = indexes map {index => (width - 1 - index / width, index % width)}
    indexSeq.toSet
  }

  def getPositionsFromBoundaries(boundaries: Iterable[Block]): Iterable[Position] = {
    boundaries flatMap getPositionsFromBoundary
  }

  def getPositionsFromBoundary(boundary: Block): Set[Position] = {
    val angles = Set(0, -90, 90, 180)
    val offset = angles flatMap getBoundariesFromAngle
    offset map {case ((row, col), angle) => ((boundary._1 - row, boundary._2 - col), angle)}
  }

  def getBlocksFromAngle(angle: Int): Set[Block] = angle match {
    case -90 => blocks map rotateLeft
    case  90 => blocks map rotateRight
    case 180 => blocks map flip
    case   _ => blocks
  }

  def getBlocksFromPosition(position: Position): Set[Block] = {
    val ((row, col), angle) = position
    val rotated = getBlocksFromAngle(angle)
    rotated map {case (blockX, blockY) => (row + blockX, col + blockY)}
  }

  private def getBoundariesFromAngle(angle: Int): Iterable[Position] = {
    val grouped = getBlocksFromAngle(angle) groupBy {_._2}
    grouped.values map {col => (col minBy {_._1}, angle)}
  }

  private def rotateLeft(block: Block): Block = (block._2, width - block._1 - 1)

  private def rotateRight(block: Block): Block = (width - block._2 - 1, block._1)

  private def flip(block: Block): Block = (width - block._1 - 1, width - block._2 - 1)
}
