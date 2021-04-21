package activeOverhypoLearnerSpec

import priors._
import learner._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalactic._
import TripleEquals._
import Tolerance._

class PriorsSpec extends AnyFlatSpec {
  // set tolerance for comparing Doubles (via ===)
  implicit val doubleEquality = TolerantNumerics.tolerantDoubleEquality(0.00001)

  // not pragmatic
  val ssF = SimpleSpace(0.9, Set("0", "1", "2").map(Block(_)), false)
  // pragmatic
  val ssT = SimpleSpace(0.9, Set("0", "1", "2").map(Block(_)), true)

  "SimpleSpace" should "create a joint distribution that sums to 1" in {
    assert(ssF.hypsDist.atoms.values.sum === 1.0)
    assert(ssT.hypsDist.atoms.values.sum === 1.0)
  }

  "SimpleSpace" should "create a joint distribution whose marginal disj/conj distributions equal the initial priorPDisj/priorPConj parameters" in {
    assert(ssF.hypsDist.atoms.filter(_._1.fform == ssF.disj).map(_._2).sum === ssF.priorPDisj)
    assert(ssF.hypsDist.atoms.filter(_._1.fform == ssF.conj).map(_._2).sum === ssF.priorPConj)

    assert(ssT.hypsDist.atoms.filter(_._1.fform == ssT.disj).map(_._2).sum === ssF.priorPDisj)
    assert(ssT.hypsDist.atoms.filter(_._1.fform == ssT.conj).map(_._2).sum === ssF.priorPConj)
  }

}
