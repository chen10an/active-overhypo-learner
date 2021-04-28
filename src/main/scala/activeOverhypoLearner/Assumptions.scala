package learner
import utils._

trait structInfoGain extends Learner {

  override def comboInfoGain(combo: Set[Block]): Double = {
    // expected information gain for an intervention: sum of info gain for each possible outcome (pos/neg) weighted by the probability of that outcome
    val outcomeDist = outcomeMarginal(combo)
    val priorStructEntropy = structMarginal(hypsDist).entropy
    outcomeDist.atoms.map(tup => (priorStructEntropy - structMarginal(posterior(tup._1)).entropy) * tup._2).sum
  }

  override def comboEntropy(combo: Set[Block]): Double = {
    // expected entropy of the posterior distribution after intervening with `combo`
    val outcomeDist = outcomeMarginal(combo)
    outcomeDist.atoms.map(tup => structMarginal(posterior(tup._1)).entropy * tup._2).sum
  }
}
