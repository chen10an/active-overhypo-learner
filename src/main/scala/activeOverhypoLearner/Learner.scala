package learner

import utils._

// TODO: first integrate learner trait without any additional assumptions and then make sure existing unit tests work
// TODO: rename likelihood to something else cause that function doesn't actually calculate the likelihood; it's also misleading to call the "likelihood" function to calculate outcomeMarginal

trait Learner {
  // base class for implementing a Bayesian learner that can choose interventions wrt information gain

  // ***abstract***
  // current joint distribution over structures and forms
  def hypsDist: Dist[Hyp]

  // should return a learner with an updated hypsDist over the same joint hypotheses (forms and blickets) used in the current Learner
  def update(events: Vector[Event]): Learner

  // should return a learner with the same marginal fform Dist while reinitializing structures (and the joint dist) using new blocks allBlocks
  def transfer(allBlocks: Set[Block]): Learner

  // ***concrete***
  val fformBinSize: Double = 1.0  // override to get histogram approximation of the marginal fform entropy

  lazy val allFforms: Set[Fform] = hypsDist.atoms.keys.map(_.fform).toSet
  lazy val allStructs: Set[Set[Block]] = hypsDist.atoms.keys.map(_.blickets).toSet

  // in the pragmatic case:
  lazy val maxStructSize = allStructs.map(_.size).max
  // the largest struct in a pragmatic hypothesis space should contain all blocks in the phase
  lazy val allBlocks: Set[Block] = allStructs.filter(_.size == maxStructSize).flatten
  // the possible pragmatic structs is not the same as the possible interventions (all combos of allBlocks)
  lazy val allInterventions: Set[Set[Block]] = allBlocks.subsets().toSet
  // but below, where there is no need to make a distinction between struct vs. intervention, both are generalized as "combo", i.e., a combination of blocks

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
    // the returned marginal distribution supports histogram approximations of entropy, depending on the (overridden) value of fformBinSize

    Dist(allFforms.map(fform => (fform, jointDist.atoms.filter(_._1.fform == fform).map(_._2).sum)).toMap, fformBinSize)
  }

  def structMarginal(jointDist: Dist[Hyp]): Dist[Set[Block]] = {
    // for each structure (i.e. set of blickets), get its marginal probability by summing the joint probabilities over all forms
    Dist(allStructs.map(combo => (combo, jointDist.atoms.filter(_._1.blickets == combo).map(_._2).sum)).toMap)
  }

  lazy val priorFformMarginal: Dist[Fform] = fformMarginal(hypsDist)
  lazy val priorStructMarginal: Dist[Set[Block]] = structMarginal(hypsDist)

  def outcomeMarginal(combo: Set[Block]): Dist[Event] = {

    val possibleOutcomes = Vector(true, false)
    // possible events conditioned on the intervention (block combo without the outcome)
    val possibleEvents = possibleOutcomes.map(o => Event(combo, o))

    // for each (conditioned on the intervention) possible event, its probability is calculated by marginalizing over all joint hypotheses
    // i.e. multiplying the likelihood of the event given a joint hypothesis with the prior of that joint hypothesis and then summing over all joint hypotheses
    // TODO: "likelihood" is a misleading method name; it's actually the raw functional form (or 1-functional form) without the additional multiplicative constant! the method usage is correct here but the method name is misleading
    val outcomeDist = Dist(possibleEvents.map(e => (e, hypsDist.atoms.map(tup => likelihood(e, tup._1) * tup._2).sum)).toMap)
    // note: the marginal probability of an outcome is the same as the normalizing constant for doing a Bayesian update where the outcome actually occured
    
    outcomeDist
  }

  def infoGain(hypsPost: Dist[Hyp], over: String = "joint"): Double = {
    // information gain: entropy of prior - entropy of posterior
    // over parameter chooses what to gain information over: full joint distribution, or the marginal distribution of fform or struct

    val possible_overs = Set("joint", "fform", "struct") 
    assert(possible_overs(over))  // check containment

    var ret: Double = Double.NaN

    if (over == "joint") {
      ret = hypsDist.entropy - hypsPost.entropy
    } else if (over == "fform") {
      ret = priorFformMarginal.entropy - fformMarginal(hypsPost).entropy
    } else if (over == "struct") {
      ret = priorStructMarginal.entropy - structMarginal(hypsPost).entropy
    }

    ret
  }

  def comboInfoGain(combo: Set[Block], over: String = "joint"): Double = {
    // expected information gain for an intervention: sum of info gain for each possible outcome (pos/neg) weighted by the probability of that outcome
    // use the over parameter to specify which distribution (joint, marginal form, or marginal struct) to calculate entropies on

    val outcomeDist = outcomeMarginal(combo)
    outcomeDist.atoms.map(tup => infoGain(posterior(tup._1), over) * tup._2).sum
  }

  lazy val comboValMap = allInterventions.map(combo => (combo, comboInfoGain(combo, "joint"))).toMap
  lazy val fformComboValMap = allInterventions.map(combo => (combo, comboInfoGain(combo, "fform"))).toMap
  lazy val structComboValMap = allInterventions.map(combo => (combo, comboInfoGain(combo, "struct"))).toMap

  // intervention that maximizes information gain:
  // lazy val maxComboVals = {
  //   val maxVal = comboValMap.values.max
  //   comboValMap.filter(_._2 == maxVal)
  // }

  // note that this uses the JOINT comboValMap by default:
  lazy val comboRanks: Map[Set[Block], Integer] = {
    // Rank each combo (i.e. intervention) so that the highest info gain combo has rank 1,
    // the second highest has rank 2 and so on.
    // Multiple combos can share the same rank if they have the same info gain value (with precision/rounding tolerance).
    val highestFirstDistinctVals: Vector[BigDecimal] = comboValMap.values.map(NumberUtils.round(_)).toVector.distinct.sorted.reverse

    comboValMap.map(tup => tup._1 -> (highestFirstDistinctVals.indexOf(NumberUtils.round(tup._2)) + 1))
  }

  def chooseIntervention(): Set[Block] = {
    // default behavior of this base Learner is to sample one of the rank 1 (information-maximizing) interventions (there can be multiple)

    val rank1Combos: IndexedSeq[Set[Block]] = comboRanks.filter(_._2 == 1).keys.toIndexedSeq
    val sampledCombo: Set[Block] = RandUtils.sampleRandomElement(rank1Combos)

    sampledCombo
  }

}
