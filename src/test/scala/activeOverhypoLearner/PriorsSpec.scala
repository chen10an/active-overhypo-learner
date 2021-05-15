package activeOverhypoLearnerSpec

import utils._
import learner._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalactic._
import TripleEquals._
import Tolerance._

class PriorsSpec extends AnyFlatSpec {
  // set tolerance for comparing Doubles (via ===)
  implicit val doubleEquality = TolerantNumerics.tolerantDoubleEquality(0.00001)

  val priorPDisj = 0.9
  val priorPConj = 0.1

  // not pragmatic
  val ssF = PriorMaker.makeDisjConjPrior(priorPDisj, Set("0", "1", "2").map(Block(_)), false)
  // pragmatic
  val ssT = PriorMaker.makeDisjConjPrior(priorPDisj, Set("0", "1", "2").map(Block(_)), true)

  "SimpleSpace" should "create a joint distribution that sums to 1" in {
    assert(ssF.atoms.values.sum === 1.0)
    assert(ssT.atoms.values.sum === 1.0)
  }

  "SimpleSpace" should "create a joint distribution whose marginal disj/conj distributions equal the initial priorPDisj/priorPConj parameters" in {
    assert(ssF.atoms.filter(_._1.fform.name == "disj").map(_._2).sum === priorPDisj)
    assert(ssF.atoms.filter(_._1.fform.name == "conj").map(_._2).sum === priorPConj)

    assert(ssT.atoms.filter(_._1.fform.name == "disj").map(_._2).sum === priorPDisj)
    assert(ssT.atoms.filter(_._1.fform.name == "conj").map(_._2).sum === priorPConj)
  }

}
