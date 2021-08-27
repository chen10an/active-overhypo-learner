package learner

import utils._
import scala.util.Random

object PriorMaker {
  val random = new Random(0)

  val disj = Fform("disj", (n: Int) => if(n >= 1) 1.0 else 0.0)
  val noisy_disj = Fform("noisy_disj", (n: Int) => {
    if (n == 1) {
      0.75  // noise
    } else if (n > 1) {
        1.0
    } else {
        0.0
    }})

  val conj = Fform("conj", (n: Int) => if(n >= 2) 1.0 else 0.0)
  val noisy_conj = Fform("noisy_conj", (n: Int) => {
    if (n == 2) {
      0.75  // noise
    } else if (n > 2) {
        1.0
    } else {
        0.0
    }})

  val conj3 = Fform("conj3", (n: Int) => if(n >= 3) 1.0 else 0.0)
  val noisy_conj3 = Fform("noisy_conj3", (n: Int) => {
    if (n == 3) {
      0.75  // noise
    } else if (n > 3) {
        1.0
    } else {
        0.0
    }})

  def makeDisjPrior(allBlocks: Set[Block], isPragmatic: Boolean): Dist[Hyp] = {
    val fformDist = Dist(Map(disj -> 1.0))

    makeJointDist(allBlocks, isPragmatic, fformDist)
  }

  def makeDisjConjPrior(priorPDisj: Double, allBlocks: Set[Block], isPragmatic: Boolean): Dist[Hyp] = {
    val priorPConj = {
      assert(priorPDisj >= 0.0 && priorPDisj <= 1.0)
      1.0-priorPDisj
    }

    val fformDist = Dist(Map(disj -> priorPDisj, conj -> priorPConj))

    makeJointDist(allBlocks, isPragmatic, fformDist)
  }

  def makeSigmoidPrior(biasGainToP: Map[(Double, Double), Double], allBlocks: Set[Block], isPragmatic: Boolean): Dist[Hyp] = {
    assert(NumberUtils.round(biasGainToP.values.sum).toDouble == 1.0)

    // get all combos of b and g and their joint probability (via multiplication, assuming prior independence)
    val fformDist = Dist(biasGainToP.map{case (bg, p) => {Fform(s"${bg._1}, ${bg._2}", (n: Int) => sigmoid(n, bg._1, bg._2)) -> p}})

    makeJointDist(allBlocks, isPragmatic, fformDist)
  }

  def makeEnumeratedPrior(fformToP: Map[Fform, Double], allBlocks: Set[Block], isPragmatic: Boolean): Dist[Hyp] = {
    // this function makes it easy to input any arbitrarily enumerated space of functional forms (with associated probabilities), e.g. {disj, conj, conj3, noisy_disj, noisy_conj, noisy_conj3}

    assert(NumberUtils.round(fformToP.values.sum).toDouble == 1.0)

    val fformDist = Dist(fformToP)

    makeJointDist(allBlocks, isPragmatic, fformDist)
  }

  private def sigmoid(nblickets: Int, bias: Double, gain: Double): Double = {
    1.0 / (1.0 + math.exp(-(gain * (nblickets - bias))))
  }

  private def makeJointDist(allBlocks: Set[Block], isPragmatic: Boolean, fformDist: Dist[Fform]): Dist[Hyp] = {

     val allFforms = fformDist.atoms.keys.toSet

    // all possible combinations/subsets of blocks
    val allStructs = allBlocks.subsets().toVector.map(_.asInstanceOf[Set[Block]])
    val uniformStructDist = Dist(allStructs.map(blickets => (blickets, 1.0)).toMap) // unnormalized

    // if isPragmatic, then only consider joint hypotheses where the blickets have a nonzero chance of activating the blicket machine
    val allHyps: Vector[Hyp] = allFforms.map(fform => allStructs.filter(x => !isPragmatic || (fform.f(x.size) > 0.0)).map(combo => Hyp(combo, fform))).reduce(_ ++ _)

    val unscaledJointDist = Dist[Hyp](allHyps.map(h => (h, uniformStructDist.atoms(h.blickets))).toMap)

    // and then multiply the structure probabilities with a scale factor such that the initial marginal form probability ratios hold
    // calculate the scale factor x, where x*sum(structures_for_fform) = fformP (assume independence between form and structure)
    val scaleFactors: Map[Fform, Double] = fformDist.atoms.map{case (fform, p) => {fform -> p/unscaledJointDist.atoms.filter(_._1.fform == fform).map(_._2).sum}}

    // for each joint hypo, scale the structure probability wrt the form and then normalize 
    Dist(unscaledJointDist.atoms.map{case (hyp, p) => {hyp -> p*scaleFactors(hyp.fform)}}).normalize
  }
}
