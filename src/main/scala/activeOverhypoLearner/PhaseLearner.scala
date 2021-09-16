package learner
import utils._

case class PhaseLearner(hypsDist: Dist[Hyp]) extends Learner {
  def update(events: Vector[Event]): PhaseLearner = {
    // return a same-phase learner with where the current PhaseLearner's **joint** hypsDist has been updated by `events`
    // this same-phase learner would use the same hypothesis space (and thus the same blocks and blickets) as the current PhaseLearner
    PhaseLearner(multiPosterior(events))
  }

  def transfer(allBlocks: Set[Block]): PhaseLearner = {
    // return a different-phase learner with the same **marginal** distribution over functional forms
    // this different-phase learner can use a different set of blocks (i.e., a different space of causal structures)

    assert(NumberUtils.round(priorFformMarginal.atoms.values.sum) == 1.0 || NumberUtils.round(priorFformMarginal.atoms.values.sum) == 0.0)
    val newJointDist = PriorMaker.makeJointDist(allBlocks, false, priorFformMarginal)

    PhaseLearner(newJointDist)
  }
}


