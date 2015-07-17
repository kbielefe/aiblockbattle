import org.scalatest._

class AiBattleTest extends FlatSpec with Matchers {

  val minimax = new Minimax[Int](_ < _)

  "Infinity" should "be equal to Infinity" in {
    assert(minimax.Infinity == minimax.Infinity)
  }
}
