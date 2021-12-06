package learner
import utils._

trait point1FformBinSize extends PhaseLearner {
  // perform histogram approximations of the marginal fform entropy using a bin size of 0.1
  // this is targeted at being used with a sigmoid prior space, with 0.1 bins for bias and 1.0 bins for gain --> final bin area/size=0.1

  override val fformBinSize: Double = 0.1

  override def update(events: Vector[Event]): PhaseLearner with point1FformBinSize = {
     new PhaseLearner(multiPosterior(events)) with point1FformBinSize
  }

  override def transfer(allBlocks: Set[Block], currWeight: Double = 1.0, otherFformDist: Dist[Fform] = Dist(Map.empty[Fform, Double])): PhaseLearner with point1FformBinSize = {
    val newJointDist: Dist[Hyp] = super.transfer(allBlocks).hypsDist

    new PhaseLearner(newJointDist) with point1FformBinSize
  }
}
