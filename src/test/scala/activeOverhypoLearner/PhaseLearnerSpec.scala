package activeOverhypoLearnerSpec

import learner._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalactic._
import TripleEquals._
import Tolerance._

class PhaseLearnerSpec extends AnyFlatSpec {
  // set tolerance for comparing Doubles (via ===)
  implicit val doubleEquality = TolerantNumerics.tolerantDoubleEquality(0.00001)

  // prepare simple test case with 2 possible functional forms (conj, disj) and 2 blocks (A, B)
  val conj = Fform("conj", (n: Int) => if(n >= 2) 1.0 else 0.0)
  val disj = Fform("disj", (n: Int) => if(n >= 1) 1.0 else 0.0)
  val priorPDisj = 0.5
  val priorPConj = 1-priorPDisj
  val fformDist = Dist(Map(disj -> priorPDisj, conj -> priorPConj))
  val allBlocks = Set("A", "B").map(Block(_))

  // all possible combos
  val comboAB = Set(Block("A"), Block("B"))
  val comboA = Set(Block("A"))
  val comboB = Set(Block("B"))
  val comboEmpty = Set.empty[Block]
  val allCombos = Vector(comboAB, comboA, comboB, comboEmpty)
  val uniformStructPrior = Dist[Set[Block]](allCombos.map(blickets => (blickets, 1.0)).toMap).normalize

  // initialize Bayesian agent
  val testLearner = PhaseLearner(fformDist, uniformStructPrior, allBlocks)

  val posEvent = Event(comboA, true)
  val negEvent = Event(comboA, false)
  val posPosteriorDist = testLearner.posterior(posEvent)
  val negPosteriorDist = testLearner.posterior(negEvent)

  // hypotheses that have a likelihood of 1.0 for the positive event
  val posLikOneHyps: Vector[Hyp] = Vector(
      Hyp(comboA, disj),
      Hyp(comboAB, disj)
  )

  // hypotheses that have a likelihood of 1.0 for the negative event
  val negLikOneHyps: Vector[Hyp] = Vector(
      Hyp(comboEmpty, disj),
      Hyp(comboB, disj),
      Hyp(comboEmpty, conj),
      Hyp(comboA, conj),
      Hyp(comboB, conj),
      Hyp(comboAB, conj)
  )

  it should "initialize the joint distribution where probabilities are the multiplicative product and values are the cartesian product of forms and structures" in {
    val hyps = testLearner.hypsDist.atoms.keySet
    val probs = testLearner.hypsDist.atoms.values

    // all cartesian product values are present
    assert(hyps.equals((posLikOneHyps ++ negLikOneHyps).toSet))

    // uniform joint probability
    assert(probs.forall(_ === 1.0/8))
    // total probability should be 1
    assert(probs.sum === 1.0) 
  }

  it should "calculate correct marginal probabilities for forms and structures" in {
    val structDist = testLearner.structMarginal(testLearner.hypsDist)
    val fformDist = testLearner.fformMarginal(testLearner.hypsDist)

    assert(structDist.atoms.values.forall(_ === 1.0/4))
    assert(fformDist.atoms.values.forall(_ === 1.0/2))
  }

  it should "calculate correct likelihood values" in {

    // hypotheses that have likelihood 1 for the positive event should have likelihood 0 for the negative event
    assert(posLikOneHyps.forall(testLearner.likelihood(posEvent, _) === 1.0))
    assert(posLikOneHyps.forall(testLearner.likelihood(negEvent, _) === 0.0))

    // hypotheses that have likelihood 1 for the negative event should have likelihood 0 for the positive event
    assert(negLikOneHyps.forall(testLearner.likelihood(negEvent, _) === 1.0))
    assert(negLikOneHyps.forall(testLearner.likelihood(posEvent, _) === 0.0))
  }

  it should "calculate correct posterior values for single and multiple events" in {

    // total probability should be 1
    assert(posPosteriorDist.atoms.values.sum === 1.0)
    assert(negPosteriorDist.atoms.values.sum === 1.0)

    // hypothesis that have likelihood 1 for pos event should have 1/2 posterior
    assert(posLikOneHyps.forall(posPosteriorDist.atoms(_) === 1.0/2))
    assert(posLikOneHyps.forall(negPosteriorDist.atoms(_) === 0.0))

    // hypothesis that have likelihood 1 for neg event should have 1/6 posterior
    assert(negLikOneHyps.forall(negPosteriorDist.atoms(_) === 1.0/6))
    assert(negLikOneHyps.forall(posPosteriorDist.atoms(_) === 0.0))

    // suppose we observe A,F and B,F
    val multiPostAB = testLearner.multiPosterior(Vector(Event(comboA, false), Event(comboB, false)))
    val multiPostBA = testLearner.multiPosterior(Vector(Event(comboB, false), Event(comboA, false)))

    // exchangable
    assert(multiPostAB.equals(multiPostAB))

    assert(multiPostAB.atoms.values.sum === 1.0)
    val multiConsistentHyps = Vector(
      Hyp(comboEmpty, disj),
      Hyp(comboEmpty, conj),
      Hyp(comboA, conj),
      Hyp(comboB, conj),
      Hyp(comboAB, conj)
    )
    assert(multiPostAB.atoms.forall(tup => !multiConsistentHyps.contains(tup._1) || tup._2 === 1.0/5))
  }

  it should "calculate correct (log base 2) entropy values" in {

    // -log_2(1/8)
    assert(testLearner.hypsDist.entropy === 3.0)

    // -log_2(1/2)
    assert(posPosteriorDist.entropy === 1.0)

    // -log_2(1/6)
    assert(negPosteriorDist.entropy === 2.585 +- 0.001)
  }

  it should "calculate correct marginal outcome (T/F) probabilities" in {
    // this only covers comboA
    val outcomeDist = testLearner.outcomeMarginal(comboA)
    assert(outcomeDist.atoms.values.sum === 1.0)

    // 2 hypotheses are consistent with A, true; the remaining 6 are consistent with A, false
    assert(outcomeDist.atoms(Event(comboA, true)) === 2.0/8)
    assert(outcomeDist.atoms(Event(comboA, false)) === 6.0/8)
  }

  it should "map combos to correct expected info gain and rank (highest E[info gain] corresponds to rank 1)" in {

    // info gain calculation check for combo A only: [info gain of true outcome]*[p of true outcome] + [info gain of false outcome]*[p of false outcome], conditioned on the A intervention
    assert(testLearner.comboInfoGain(comboA) === (3.0-1.0)*2.0/8 + (3.0-2.585)*6.0/8 +- 0.001) 

    // rank check for all combos
    assert(testLearner.comboRanks(comboAB) == 1)
    assert(testLearner.comboRanks(comboA) == 2)
    assert(testLearner.comboRanks(comboB) == 2)
    assert(testLearner.comboRanks(comboEmpty) == 3)
  }

  it should "map combos to their softmax probabilities, parameterized by beta (inverse temperature)" in {

  }
}
