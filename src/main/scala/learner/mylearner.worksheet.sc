case class Fform(name: String, f: Int => Double) {
  override def toString = name
}
// the function within each Fform object takes the number n of present blickets as input
val conj = Fform("conj", (n: Int) => if(n >= 2) 1.0 else 0.0)
val disj = Fform("disj", (n: Int) => if(n >= 1) 1.0 else 0.0)

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

  def prettyPrint = {
    atoms.mkString("\n")
  }
}

case class PhaseLearner(fformDist: Dist[Fform], allBlocks: Set[Block]) {
  val allCombos = allBlocks.subsets().toIndexedSeq.map(_.asInstanceOf[Set[Block]])
  val possibleOutcomes = Vector(true, false)
  val allEvents = allCombos.map(combo => Event(combo, true)) ++ allCombos.map(combo => Event(combo, false))
  // make joint hypotheses of functional form with the phase-specific structure (blocks and blickets)
  val allHyps = allCombos.map(combo => Hyp(combo, disj)) ++ allCombos.map(combo => Hyp(combo, conj))

  val uniformStructPrior = Dist[Set[Block]](allCombos.map(blickets => (blickets, 1.0)).toMap)
  val hypsDist = {
    // scale each structure probability with its corresponding form probability and then normalize
    val dist = Dist[Hyp](allHyps.map(h => (h, uniformStructPrior.atoms(h.blickets) * fformDist.atoms(h.fform))).toMap).normalize
    // check that all probabilities sum to 1 after normalizing:
    assert(dist.atoms.map(h => h._2).sum == 1)
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
    assert(dist.atoms.map(_._2).sum == 1.0)
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

    assert(outcomeDist.atoms.values.sum == 1.0)

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

  def update(events: Vector[Event], allBlocks: Set[Block]): PhaseLearner = {
    // sequentially update the joint distribution with all events and then return the final marginal form distribution wrapped within a new PhaseLearner object
    val multiPost = multiPosterior(events)
    assert(multiPost.atoms.values.sum == 1.0)

    val postFformDist = fformMarginal(multiPost)
    assert(postFformDist.atoms.values.sum == 1.0)

    PhaseLearner(postFformDist, allBlocks)
  }
}

// prepare inputs to the constructor
val allFforms = Set(disj, conj)
val priorPDisj = 0.5
val priorPConj = 1-priorPDisj
assert(priorPDisj + priorPConj == 1)

val fformDist = Dist(Map(disj -> priorPDisj, conj -> priorPConj))
val allBlocks = Set("A", "B").map(Block(_))

val toyLearner = PhaseLearner(fformDist, allBlocks)

// observed event:
val event = Event(Set(Block("A")), false)
val hyp = toyLearner.allHyps(1)
toyLearner.likelihood(event, hyp)

val hypsPost = toyLearner.posterior(event)
hypsPost.atoms(hyp)
toyLearner.fformMarginal(hypsPost).atoms(conj)
toyLearner.fformMarginal(hypsPost).atoms(disj)

val event2 = Event(Set(Block("B")), false)
val hypsPost2 = toyLearner.posterior(event2)
hypsPost2.atoms(hyp)
toyLearner.fformMarginal(hypsPost2).atoms(conj)
toyLearner.fformMarginal(hypsPost2).atoms(disj)

toyLearner.infoGain(hypsPost)
toyLearner.hypsDist.entropy
hypsPost.entropy

val event3 = Event(Set(Block("A"), Block("B")), true)
val hypsPost3 = toyLearner.posterior(event3)
hypsPost3.atoms(hyp)
toyLearner.fformMarginal(hypsPost3).atoms(conj)
toyLearner.fformMarginal(hypsPost3).atoms(disj)

val events = Vector(event, event3)

toyLearner.multiPosterior(events)

val nextLearner = toyLearner.update(events, allBlocks)

nextLearner.fformDist.atoms(disj)
nextLearner.fformDist.atoms(conj)
nextLearner.hypsDist

nextLearner.maxComboVals

// todo: check multiPosterior calculation (write unit test)
