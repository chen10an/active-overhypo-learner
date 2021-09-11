// given that the obvious time bottleneck is on calculating fformComboValMap (and generally things related to the marginal distribution of fforms), this file tests different implementations to reduce that bottleneck

package learner
import utils._
import utils.TimeUtils.time

trait Old extends PhaseLearner {
  // just to have a record of the initial slow implementation

  override def fformMarginal(jointDist: Dist[Hyp]): Dist[Fform] = {
    // for each functional form, get its marginal probability by summing the joint probabilities over all structures
    // the returned marginal distribution supports histogram approximations of entropy, depending on the (overridden) value of fformBinSize

    Dist(allFforms.map(fform => (fform, jointDist.atoms.filter(_._1.fform == fform).map(_._2).sum)).toMap, fformBinSize)
  }

  // structMarginal also looked similar
}

trait Values extends PhaseLearner {
  // .values instead of .map(_._2)

  override def fformMarginal(jointDist: Dist[Hyp]): Dist[Fform] = {
    Dist(allFforms.map(fform => (fform, jointDist.atoms.filter(_._1.fform == fform).values.sum)).toMap, fformBinSize)
  }
}

trait Collect extends PhaseLearner {
  // .collect instead of .map(_._2)

  def getFformHyps(fform: Fform): Vector[Hyp] = {
    allStructs.map(blickets => Hyp(blickets, fform))
  }

  override def fformMarginal(jointDist: Dist[Hyp]): Dist[Fform] = {
    val fformAtoms: Map[Fform, Double] = allFforms.map(fform => fform -> (getFformHyps(fform) collect jointDist.atoms).sum).toMap

    Dist(fformAtoms, fformBinSize)
  }

  def getStructHyps(blickets: Set[Block]): Vector[Hyp] = {
    allFforms.map(fform => Hyp(blickets, fform))
  }

  override def structMarginal(jointDist: Dist[Hyp]): Dist[Set[Block]] = {
    val structAtoms: Map[Set[Block], Double] = allStructs.map(blickets => blickets -> (getStructHyps(blickets) collect jointDist.atoms).sum).toMap

    Dist(structAtoms)
  }
}

trait OneLoop extends PhaseLearner {
  // sum up both form and struct marginal probabilities in the same loop that calculates entropy, normalizing constant, etc. in Dist

  // only _slightly_ better than Collect, like 1-2sec for calculating fformComboValMap + structComboValMap

  override def fformMarginal(jointDist: Dist[Hyp]): Dist[Fform] = {
    Dist(jointDist.fformMarginalAtoms, fformBinSize) 
  }
  override def structMarginal(jointDist: Dist[Hyp]): Dist[Set[Block]] = {
    Dist(jointDist.structMarginalAtoms)
  }
}

object Profiling {
  def main(args: Array[String]): Unit = {
    val blocksMap = Map[Int, Set[Block]](
      1 -> Set("0", "1", "2").map(Block(_)),
      2 -> Set("0", "1", "2", "3", "4", "5").map(Block(_)),
    )

    val biases = (0 to 19).map(_*0.1).toVector
    val gains = (0 to 19).map(_.toDouble).toVector
    val bgs = biases.map(b => gains.map(g => (b, g))).reduce(_ ++ _)
    val bgToP: Map[(Double, Double), Double] = bgs.map(bg => (bg, 1.0/bgs.length)).toMap
    val sigmoidPrior = PriorMaker.makeSigmoidPrior(bgToP, blocksMap(2), false)

    val curr = PhaseLearner(sigmoidPrior)
    val test = new PhaseLearner(sigmoidPrior) with Old

    println("Current implementation:")
    time {
      curr.fformComboValMap
      curr.structComboValMap
    }

    println("New implementation:")
    time {
      test.fformComboValMap
      test.structComboValMap
    }
    
    // val old = new PhaseLearner(sigmoidPrior) with Old
    // val values = new PhaseLearner(sigmoidPrior) with Values
    // val collect = new PhaseLearner(sigmoidPrior) with Collect

    // val oldRes = time {old.fformComboValMap}
    // val valuesRes = time {values.fformComboValMap}
    // as expected not much difference and both take forever: ~90s
    // --> caching all participants' combovals in 90*20*240/60/60 = 120hrs :(

    // val collectRes = time {collect.fformComboValMap}
    // WINNER! ~22s
    // --> caching all participants' combovals in 22*20*240/60/60 = ~30hrs
  }
}
