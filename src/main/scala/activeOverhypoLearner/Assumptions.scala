package learner
import utils._

trait structInfoGain extends Learner {

  // override def comboInfoGain(combo: Set[Block]): Double = {
  //   // expected information gain for an intervention: sum of info gain for each possible outcome (pos/neg) weighted by the probability of that outcome
  //   val outcomeDist = outcomeMarginal(combo)
  //   val priorStructEntropy = structMarginal(hypsDist).entropy
  //   outcomeDist.atoms.map(tup => (priorStructEntropy - structMarginal(posterior(tup._1)).entropy) * tup._2).sum
  // }
  // TODO: optimize wrt new implementation infoGain in Learner

   override def update(events: Vector[Event]): PhaseLearner with structInfoGain = {
     new PhaseLearner(multiPosterior(events)) with structInfoGain
  }
}

trait point1FformBinSize extends PhaseLearner {
  // perform histogram approximations of the marginal fform entropy using a bin size of 0.1
  // this is targeted at being used with a sigmoid prior space, with 0.1 bins for bias and 1.0 bins for gain --> final bin area/size=0.1

  override val fformBinSize: Double = 0.1

  override def update(events: Vector[Event]): PhaseLearner with point1FformBinSize = {
     new PhaseLearner(multiPosterior(events)) with point1FformBinSize
  }

  override def transfer(allBlocks: Set[Block]): PhaseLearner with point1FformBinSize = {
    val newJointDist: Dist[Hyp] = super.transfer(allBlocks).hypsDist

    new PhaseLearner(newJointDist) with point1FformBinSize
  }
}


// TODO: implement softmax learner by overriding chooseIntervention
