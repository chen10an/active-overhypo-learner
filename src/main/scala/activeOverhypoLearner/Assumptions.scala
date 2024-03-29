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
    val newJointDist: Dist[Hyp] = super.transfer(allBlocks, currWeight, otherFformDist).hypsDist

    new PhaseLearner(newJointDist) with point1FformBinSize
  }
}

trait point15FformBinSize extends PhaseLearner {

  override val fformBinSize: Double = 0.15

  override def update(events: Vector[Event]): PhaseLearner with point15FformBinSize = {
    new PhaseLearner(multiPosterior(events)) with point15FformBinSize
  }

  override def transfer(allBlocks: Set[Block], currWeight: Double = 1.0, otherFformDist: Dist[Fform] = Dist(Map.empty[Fform, Double])): PhaseLearner with point15FformBinSize = {
    val newJointDist: Dist[Hyp] = super.transfer(allBlocks, currWeight, otherFformDist).hypsDist

    new PhaseLearner(newJointDist) with point15FformBinSize
  }
}

trait point3FformBinSize extends PhaseLearner {

  override val fformBinSize: Double = 0.3

  override def update(events: Vector[Event]): PhaseLearner with point3FformBinSize = {
    new PhaseLearner(multiPosterior(events)) with point3FformBinSize
  }

  override def transfer(allBlocks: Set[Block], currWeight: Double = 1.0, otherFformDist: Dist[Fform] = Dist(Map.empty[Fform, Double])): PhaseLearner with point3FformBinSize = {
    val newJointDist: Dist[Hyp] = super.transfer(allBlocks, currWeight, otherFformDist).hypsDist

    new PhaseLearner(newJointDist) with point3FformBinSize
  }
}
