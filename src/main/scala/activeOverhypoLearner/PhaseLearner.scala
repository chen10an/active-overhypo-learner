package learner
import utils._

case class PhaseLearner(hypsDist: Dist[Hyp]) extends Learner {
  def update(events: Vector[Event]): PhaseLearner = {
    // return a same-phase learner with where the current PhaseLearner's **joint** hypsDist has been updated by `events`
    // this same-phase learner would use the same hypothesis space (and thus the same blocks and blickets) as the current PhaseLearner
    PhaseLearner(multiPosterior(events))
  }

  def transfer(allBlocks: Set[Block], currWeight: Double = 1.0, otherFformDist: Dist[Fform] = Dist(Map.empty[Fform, Double])): PhaseLearner = {
    // return a different-phase learner with the same or mixed **marginal** distribution over functional forms

    // allBlocks: a set of blocks (can be different from the current PhaseLearner's blocks, i.e., a different space of causal structures) for initializing a different-phase learner
    // currWeight: mixture weight to multiply with the current PhaseLearner's marginal distribution of forms; this distribution will be mixed with mixDist
    // otherFformDist: distribution of forms, has mixture weight 1-currWeight

    assert(NumberUtils.round(priorFformMarginal.atoms.values.sum) == 1.0 || NumberUtils.round(priorFformMarginal.atoms.values.sum) == 0.0)

    var mixedFformMarginal: Dist[Fform] = priorFformMarginal
    if (currWeight < 1.0) {
      // check otherFformDist has the same keys/forms as the current PhaseLearner's forms
      assert(otherFformDist.atoms.keys.toSet.equals(priorFformMarginal.atoms.keys.toSet))

      val weightedCurr: Map[Fform, Double] = priorFformMarginal.atoms.map{case (fform, p) => {fform -> p*currWeight}}
      val weightedOther: Map[Fform, Double] = otherFformDist.atoms.map{case (fform, p) => {fform -> p*(1.0 - currWeight)}}

      mixedFformMarginal = Dist(weightedCurr.map{case (fform, p) => {fform -> (p + weightedOther(fform))}})
    }

    val newJointDist = PriorMaker.makeJointDist(allBlocks, false, mixedFformMarginal)

    PhaseLearner(newJointDist)
  }
}


