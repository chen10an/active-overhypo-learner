package learner
import utils._

case class PhaseLearner(hypsDist: Dist[Hyp]) extends Learner {
  def update(events: Vector[Event]): PhaseLearner = {
    // return a same-phase learner with an updated **joint** hypsDist over the same blocks used in the current PhaseLearner
    PhaseLearner(multiPosterior(events))
  }

  // def transfer(events: Vector[Event], allBlocks: Set[Block]): PhaseLearner = {
  //   // return a different-phase learner with an updated **marginal** distribution over functional forms
  //   // this different-phase learner can use a different set of blocks (i.e., a different space of causal structures)
  //   val multiPost = multiPosterior(events)
  //   // assert(multiPost.atoms.values.sum == 1.0)

  //   val postFformDist = fformMarginal(multiPost)
  //   // assert(postFformDist.atoms.values.sum == 1.0)

  //   val uniformStructPrior = Dist[Set[Block]](allCombos.map(blickets => (blickets, 1.0)).toMap).normalize

  //   PhaseLearner(postFformDist, uniformStructPrior, allBlocks)
  // }
  // // TODO: combin update and transfer into a more compact representation
}


