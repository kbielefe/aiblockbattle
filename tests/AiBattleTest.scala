import org.scalatest._

class AiBattleTest extends FlatSpec with Matchers {

  val minimax = new Minimax[Int](_ < _)
  val finite = minimax.FiniteScore(-1)

  "Infinity" should "be greater than NegativeInfinity" in {
    assert(minimax.Infinity > minimax.NegativeInfinity)
  }

  it should "be greater than or equal to NegativeInfinity" in {
    assert(minimax.Infinity >= minimax.NegativeInfinity)
  }

  it should "not be less than NegativeInfinity" in {
    assert(!(minimax.Infinity < minimax.NegativeInfinity))
  }

  it should "not be less than or equal to NegativeInfinity" in {
    assert(!(minimax.Infinity <= minimax.NegativeInfinity))
  }

  it should "be the max of NegativeInfinity" in {
    assert(minimax.Infinity.max(minimax.NegativeInfinity) == minimax.Infinity)
  }

  it should "not be the min of NegativeInfinity" in {
    assert(minimax.Infinity.min(minimax.NegativeInfinity) == minimax.NegativeInfinity)
  }

  it should "be greater than zero" in {
    assert(minimax.Infinity > 0)
  }

  it should "be greater than or equal to zero" in {
    assert(minimax.Infinity >= 0)
  }
  
  it should "not be less than zero" in {
    assert(!(minimax.Infinity < 0))
  }

  it should "not be less than or equal to zero" in {
    assert(!(minimax.Infinity <= 0))
  }

  it should "be the max of zero" in {
    assert(minimax.Infinity.max(0) == minimax.Infinity)
  }

  it should "not be the min of zero" in {
    assert(minimax.Infinity.min(0).getNode == 0)
  }

  it should "not contain a node" in {
    an [Exception] should be thrownBy {minimax.Infinity.getNode}
  }

  "Negative Infinity" should "not be greater than Infinity" in {
    assert(!(minimax.NegativeInfinity > minimax.Infinity))
  }

  it should "not be greater than or equal to Infinity" in {
    assert(!(minimax.NegativeInfinity >= minimax.Infinity))
  }

  it should "be less than Infinity" in {
    assert(minimax.NegativeInfinity < minimax.Infinity)
  }

  it should "be less than or equal to Infinity" in {
    assert(minimax.NegativeInfinity <= minimax.Infinity)
  }

  it should "not be the max of Infinity" in {
    assert(minimax.NegativeInfinity.max(minimax.Infinity) == minimax.Infinity)
  }

  it should "be the min of Infinity" in {
    assert(minimax.NegativeInfinity.min(minimax.Infinity) == minimax.NegativeInfinity)
  }

  it should "not be greater than zero" in {
    assert(!(minimax.NegativeInfinity > 0))
  }

  it should "not be greater than or equal to zero" in {
    assert(!(minimax.NegativeInfinity >= 0))
  }
  
  it should "be less than zero" in {
    assert(minimax.NegativeInfinity < 0)
  }

  it should "be less than or equal to zero" in {
    assert(minimax.NegativeInfinity <= 0)
  }

  it should "not be the max of zero" in {
    assert(minimax.NegativeInfinity.max(0).getNode == 0)
  }

  it should "be the min of zero" in {
    assert(minimax.NegativeInfinity.min(0) == minimax.NegativeInfinity)
  }

  it should "not contain a node" in {
    an [Exception] should be thrownBy {minimax.NegativeInfinity.getNode}
  }

  "A finite score of negative one" should "not be greater than Infinity" in {
    assert(!(finite > minimax.Infinity))
  }

  it should "not be greater than or equal to Infinity" in {
    assert(!(finite >= minimax.Infinity))
  }

  it should "be less than Infinity" in {
    assert(finite < minimax.Infinity)
  }

  it should "be less than or equal to Infinity" in {
    assert(finite <= minimax.Infinity)
  }

  it should "not be the max of Infinity" in {
    assert(finite.max(minimax.Infinity) == minimax.Infinity)
  }

  it should "be the min of Infinity" in {
    assert(finite.min(minimax.Infinity) == finite)
  }

  it should "be greater than NegativeInfinity" in {
    assert(finite > minimax.NegativeInfinity)
  }

  it should "be greater than or equal to NegativeInfinity" in {
    assert(finite >= minimax.NegativeInfinity)
  }

  it should "not be less than NegativeInfinity" in {
    assert(!(finite < minimax.NegativeInfinity))
  }

  it should "not be less than or equal to NegativeInfinity" in {
    assert(!(finite <= minimax.NegativeInfinity))
  }

  it should "be the max of NegativeInfinity" in {
    assert(finite.max(minimax.NegativeInfinity) == finite)
  }

  it should "not be the min of NegativeInfinity" in {
    assert(finite.min(minimax.NegativeInfinity) == minimax.NegativeInfinity)
  }

  it should "not be greater than zero" in {
    assert(!(finite > 0))
  }

  it should "not be greater than or equal to zero" in {
    assert(!(finite >= 0))
  }
  
  it should "be less than zero" in {
    assert(finite < 0)
  }

  it should "be less than or equal to zero" in {
    assert(finite <= 0)
  }

  it should "not be the max of zero" in {
    assert(finite.max(0).getNode == 0)
  }

  it should "be the min of zero" in {
    assert(finite.min(0) == finite)
  }

  it should "contain a node" in {
    noException should be thrownBy {finite.getNode}
    assert(finite.getNode == -1)
  }
}
