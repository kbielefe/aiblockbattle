abstract class Tree[Move, State, Score](val state: State, val maximizing: Boolean) {
  type Child = (Move, Tree[Move, State, Score])

  // Abstract methods
  def scoreLessThan(left: Score, right: Score): Boolean
  def moveLessThan(left: Move, right: Move): Boolean
  def leaf: Boolean
  def generateChildren: Vector[Child]
  def generateScore: Score

  var score: Option[Score] = None
  private var children = Vector.empty[Child]

  private def minimizingSortFunction(left: Child, right: Child): Boolean = {
    val leftScored =   left._2.score.isDefined
    val rightScored = right._2.score.isDefined

    if (leftScored && rightScored)
      return scoreLessThan(left._2.score.get, right._2.score.get)

    if (leftScored && !rightScored)
      return true

    if (rightScored && !leftScored)
      return false

    moveLessThan(left._1, right._1)
  }

  private def maximizingSortFunction(left: Child, right: Child): Boolean = {
    val leftScored =   left._2.score.isDefined
    val rightScored = right._2.score.isDefined

    if (leftScored && rightScored)
      return scoreLessThan(right._2.score.get, left._2.score.get)

    if (leftScored && !rightScored)
      return true

    if (rightScored && !leftScored)
      return false

    moveLessThan(right._1, left._1)
  }

  def getChildren: Vector[Child] = {
    if (children.isEmpty)
      children = generateChildren

    if (maximizing)
      children sortWith maximizingSortFunction
    else
      children sortWith minimizingSortFunction
  }

  def getScore: Score = {
    if (!score.isDefined)
      score = Some(generateScore)
    score.get
  }

  private def indentedToString(indent: Int): Vector[String] = {
    val childStrings = children flatMap {_._2.indentedToString(indent + 2)}
    val spaces = " " * indent
    if (score.isDefined)
      Vector(spaces + state, spaces + score.get) ++ childStrings
    else
      Vector(spaces + state) ++ childStrings
  }

  override def toString: String = {
    indentedToString(0).mkString("\n")
  }
}
