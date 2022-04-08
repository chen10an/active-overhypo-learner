package learner
import utils._

object Conditioner {
  // Condition on a fixed set of events [intervention and outcome] (meant for participant intervention data) and output intermediate metrics

  def getPhaseResults(learner: PhaseLearner, events: Vector[Event]): (Array[(Array[Double], Array[Double], Double, Double, Double, Map[Fform, Double], Map[Set[Block], Double], Map[Hyp, Double], Array[Double])], Array[Set[Block]], Dist[Hyp]) = {
    // returns:
    // intermediate metrics of sequentially conditioning `learner` on each event
    // possible interventions in the same order as metric arrays (so that it's possible to match possible intervention to metric value)
    // final updated learner's hypsDist: can be used to initialize a Learner with whatever mixins --> call transfer to get a new learner with the same mixins --> put back into getPhaseResults (for conditioning the next transfer phase)

    var results = Array.ofDim[(Array[Double], Array[Double], Double, Double, Double, Map[Fform, Double], Map[Set[Block], Double], Map[Hyp, Double], Array[Double])](events.length)

    var ithLearner = learner
    val possibleInterventions:Array[Set[Block]] = learner.allInterventions.toArray  // fixed order
    
    for (i <- 0 to events.length-1) {
      printf("\r%2d", i)


      val comboToFformEIG = ithLearner.fformComboValMap
      val comboToStructEIG = ithLearner.structComboValMap
      val comboToJointEIG = ithLearner.comboValMap

      // only save the max atoms if not more than 10
      val emptyFform = Fform("NA", (n: Int) => -1.0)
      val emptyStruct = Set(Block("NA"))
      var fformMaxAtoms: Map[Fform, Double] = ithLearner.priorFformMarginal.maxAtoms
      if (fformMaxAtoms.size > 10) fformMaxAtoms = Map(emptyFform -> -1.0)
      var structMaxAtoms: Map[Set[Block], Double] = ithLearner.priorStructMarginal.maxAtoms
      if (structMaxAtoms.size > 10) structMaxAtoms = Map(emptyStruct -> -1.0)
      var jointMaxAtoms: Map[Hyp, Double] = ithLearner.hypsDist.maxAtoms
      if (jointMaxAtoms.size > 10) jointMaxAtoms = Map(Hyp(emptyStruct, emptyFform) -> -1.0)

      results(i) = (
        possibleInterventions.map(comboToFformEIG(_)).toArray,
        possibleInterventions.map(comboToStructEIG(_)).toArray,
        ithLearner.priorFformMarginal.entropy,
        ithLearner.priorStructMarginal.entropy,
        ithLearner.hypsDist.entropy,
        fformMaxAtoms,
        structMaxAtoms,
        jointMaxAtoms,
        possibleInterventions.map(comboToJointEIG(_)).toArray
      )

      ithLearner = ithLearner.update(Vector(events(i)))

    }

    (results, possibleInterventions, ithLearner.hypsDist)
  }
}
