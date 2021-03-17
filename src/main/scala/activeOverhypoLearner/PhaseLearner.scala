package activeOverhypoLearner

case class Fform(name: String, f: Int => Double) {
  override def toString = name
// the function within each Fform object takes the number n of present blickets as input
}
case class Block(name: String) {override def toString=name}

case class Event(blocks: Set[Block], outcome: Boolean)
case class Hyp(blickets: Set[Block], fform: Fform) 
case class Dist[T](atoms: Map[T, Double]) {
  lazy val entropy = {
    // todo: change to log2
    -atoms.values.map(v => if(v == 0) 0.0 else v*math.log(v)).sum
  }

  private def log2(x: Double): Double = {
    math.log(x)/math.log(2)
  }

  def normalize = {
    val denom = atoms.values.sum

    // return normalized copy
    if (denom == 0) {
      Dist(atoms.map(p => p._1 -> 0.0))  
    } else {
      Dist(atoms.map(p => p._1 -> p._2/denom))  
    }
  }
}

case class PhaseLearner(fformDist: Dist[Fform], structDist: Dist[Set[Block]], allBlocks: Set[Block]) {

  val allFforms = fformDist.atoms.keys.toSet
  val allCombos = allBlocks.subsets().toIndexedSeq.map(_.asInstanceOf[Set[Block]])
  val possibleOutcomes = Vector(true, false)
  val allEvents = allCombos.map(combo => Event(combo, true)) ++ allCombos.map(combo => Event(combo, false))

  // make joint hypotheses of functional form with the phase-specific structure (blocks and blickets)
  val allHyps = allFforms.map(fform => allCombos.map(combo => Hyp(combo, fform))).reduce(_ ++ _)

  val hypsDist = {
    // scale each structure probability with its corresponding form probability and then normalize
    val dist = Dist[Hyp](allHyps.map(h => (h, structDist.atoms(h.blickets) * fformDist.atoms(h.fform))).toMap).normalize
    // check that all probabilities sum to 1 after normalizing:
    // assert(dist.atoms.map(h => h._2).sum == 1)
    println(s"Entropy of the joint (form and structure) distribution ${dist.entropy}")
    dist
  }

  def likelihood(event: Event, hyp: Hyp): Double = {
    // likelihood of event given hyp (joint hypothesis on structure and form)
    // count the number of blickets (wrt hypothesis hyp) present in the event
    val num_blickets = event.blocks.count(b => hyp.blickets.contains(b))
    // probability of a positive outcome according to the functional form in hyp
    val positiveP = hyp.fform.f(num_blickets)
    // probablility of the actual outcome
    var outcomeP: Double = -1
    if (event.outcome) {  // positive/true outcome (machine activated)
      outcomeP = positiveP
    } else {
      outcomeP = 1.0 - positiveP
    }
    assert(outcomeP >= 0)
    outcomeP
    // The remaining likelihood calculation is omitted because it just involves multiplying outcomeP with a constant
    // which will be factored out by the subsequent normalization.
    // The constant represents the assumption that all configurations of blocks (i.e., interventions)
    // in an event are equally likely, given any hyp (joint hypothesis about structure and form).
  }

  def posterior(event: Event): Dist[Hyp] = {
    // get posterior distribution by multiplying prior with likelihood and then normalizing
     Dist(hypsDist.atoms.map(tup => {tup._1 -> tup._2*likelihood(event, tup._1)})).normalize
  }

  def multiPosterior(events: Vector[Event]): Dist[Hyp] = {
    // calculate the posterior for multiple events

    // map each hyp to the product of all likelihoods given that hyp
    val multiLikelihood = hypsDist.atoms.keys.map(hyp => (hyp, events.map(e => likelihood(e, hyp)).product)).toMap

    // for each hyp, multiply its prior with the product of all likelihoods
    // and then normalize
    Dist(hypsDist.atoms.map(tup => {tup._1 -> tup._2*multiLikelihood(tup._1)})).normalize
  }
  def fformMarginal(jointDist: Dist[Hyp]): Dist[Fform] = {
    // for each functional form, get its marginal probability by summing the joint probabilities over all structures
    val dist = Dist(allFforms.map(fform => (fform, jointDist.atoms.filter(_._1.fform == fform).map(_._2).sum)).toMap)
    // assert(dist.atoms.map(_._2).sum == 1.0)
    dist
  }

  def structMarginal(jointDist: Dist[Hyp]): Dist[Set[Block]] = {
    // for each structure (i.e. set of blickets), get its marginal probability by summing the joint probabilities over all forms
    val dist = Dist(allCombos.map(combo => (combo, jointDist.atoms.filter(_._1.blickets == combo).map(_._2).sum)).toMap)
    // assert(dist.atoms.map(_._2).sum == 1.0 +- .00001)
    dist
  }

  def infoGain(hypsPost: Dist[Hyp]): Double = {
    // information gain: entropy of prior - entropy of posterior
    hypsDist.entropy - hypsPost.entropy
  }

  def outcomeMarginal(combo: Set[Block]): Dist[Event] = {
    // possible events conditioned on the intervention (block combo without the outcome)
    val possibleEvents = possibleOutcomes.map(o => Event(combo, o))

    // for each (conditioned on the intervention) possible event, its probability is calculated by marginalizing over all joint hypotheses
    // i.e. multiplying the likelihood of the event given a joint hypothesis with the prior of that joint hypothesis and then summing over all joint hypotheses
    val outcomeDist = Dist(possibleEvents.map(e => (e, hypsDist.atoms.map(tup => likelihood(e, tup._1) * tup._2).sum)).toMap)

    // assert(outcomeDist.atoms.values.sum == 1.0)

    outcomeDist
  }

  def comboInfoGain(combo: Set[Block]): Double = {
    // expected information gain for an intervention: sum of info gain for each possible outcome (pos/neg) weighted by the probability of that outcome
    val outcomeDist = outcomeMarginal(combo)
    outcomeDist.atoms.map(tup => infoGain(posterior(tup._1)) * tup._2).sum
  }

  def comboEntropy(combo: Set[Block]): Double = {
    // expected entropy of the posterior distribution after intervening with `combo`
    val outcomeDist = outcomeMarginal(combo)
    outcomeDist.atoms.map(tup => posterior(tup._1).entropy * tup._2).sum
  }

  lazy val comboValMap = allCombos.map(combo => (combo, comboInfoGain(combo))).toMap
  // intervention that maximizes information gain:
  lazy val maxComboVals = {
    val maxVal = comboValMap.values.max
    comboValMap.filter(_._2 == maxVal)
  }

  lazy val comboEntropies = allCombos.map(combo => (combo, comboEntropy(combo))).toMap 

  def update(events: Vector[Event]): PhaseLearner = {
    // return a same-phase learner with an updated **joint** hypsDist over the same blocks used in the current PhaseLearner
    val multiPost = multiPosterior(events)
    val postFformDist = fformMarginal(multiPost)
    val postStructDist = structMarginal(multiPost)

    PhaseLearner(postFformDist, postStructDist, allBlocks)
  }

  def transfer(events: Vector[Event], allBlocks: Set[Block]): PhaseLearner = {
    // return a different-phase learner with an updated **marginal** distribution over functional forms
    // this different-phase learner can use a different set of blocks (i.e., a different space of causal structures)
    val multiPost = multiPosterior(events)
    // assert(multiPost.atoms.values.sum == 1.0)

    val postFformDist = fformMarginal(multiPost)
    // assert(postFformDist.atoms.values.sum == 1.0)

    val uniformStructPrior = Dist[Set[Block]](allCombos.map(blickets => (blickets, 1.0)).toMap)

    PhaseLearner(postFformDist, uniformStructPrior, allBlocks)
  }
  // TODO: combin update and transfer into a more compact representation
}


