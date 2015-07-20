import org.scalatest._

class PieceTest extends FlatSpec with Matchers {
  val piece = new Piece(" X XXX   ", 'T')

  "Piece T" should "have correct blocks" in {
    piece.blocks shouldBe Set((1, 0), (1, 1), (1, 2), (2, 1))
  }

  it should "have correct left rotation" in {
    piece.getBlocksFromAngle(-90) shouldBe Set((1, 0), (0, 1), (1, 1), (2, 1))
  }

  it should "have correct right rotation" in {
    piece.getBlocksFromAngle(90) shouldBe Set((1, 2), (0, 1), (1, 1), (2, 1))
  }

  it should "have correct flip" in {
    piece.getBlocksFromAngle(180) shouldBe Set((0, 1), (1, 0), (1, 1), (1, 2))
  }

  it should "have correct blocks for position" in {
    piece.getBlocksFromPosition(((10, 0), 0)) shouldBe Set((11, 0), (11, 1), (11, 2), (12, 1))
  }

  it should "have correct positions for boundary (0, 0)" in {
    piece.getPositionsFromBoundary((0, 0)) shouldBe Set(((-1,-2),90), ((0,-1),90), ((0,-1),180), ((-1,0),180), ((-1,0),-90), ((-1,-2),180), ((-1,0),0), ((0,-1),-90), ((-1,-1),0), ((-1,-2),0))
  }
}
