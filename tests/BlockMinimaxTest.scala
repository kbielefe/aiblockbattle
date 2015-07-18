import org.scalatest._

class MockMetric(
  loseInOffset: Int,
  lostGameMock: Boolean)
  extends 
  Metric(Set[(Int, Int)](),
         Set[((Int, Int), Int)](),
         new Field("0,0;0,0"),
         new Piece("", 'I'),
         ((0, 0), 0),
         0) {
  override def loseInX(x: Int): Int = loseInOffset
  override lazy val lostGame: Boolean = lostGameMock
}

class BlockMinimaxTest extends FlatSpec with Matchers {
  "Less than" should "prioritize on lost game" in {
    val lost    = new MockMetric(0, true)
    val notLost = new MockMetric(1, false)
    BlockMinimax.lt(lost, notLost) shouldBe true
  }
}
