import org.scalatest._

object testMinimax extends Minimax[Int, Int] {
  def lt(left: Int, right: Int): Boolean = left < right
  def terminal(node: Int): Boolean = node >= 8
  def getChildren(node: Int): List[Int] = List(node*2+1, node*2)
  def nextLevelMaximizes(node: Int): Boolean = node != 2 && node != 3

  def heuristic(node: Int): Infinite = {
    if (node == 12 || node == 10 || node == 8)
      throw new Exception("Did not prune")

    Finite(node)
  }
}

class MinimaxTest extends FlatSpec with Matchers {
  "Minimax" should "find the best score" in {
    testMinimax.search(1, 3) should be (14)
  }

  it should "limit the depth" in {
    testMinimax.search(1, 10) should be (14)
  }

  it should "respect the passed in depth" in {
    testMinimax.search(1, 2) should be (7)
  }

  it should "prune" in {
    noException should be thrownBy testMinimax.search(1, 3)
  }
}
