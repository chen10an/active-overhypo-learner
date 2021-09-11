package learner

import utils._

case class Simulator(trueBlickets: Set[Block], trueForm: Fform) {

  def run(learner: Learner, nSimulations: Int, nInterventions: Int): Array[Array[(Event, Double, Map[Hyp, Double], Map[Fform, Double], Map[Set[Block], Double])]] = {

    // track the event and the posterior values following that event (entropy, max joint hyp, max form, max struct)
    var sims = Array.ofDim[(Event, Double, Map[Hyp, Double], Map[Fform, Double], Map[Set[Block], Double])](nSimulations, nInterventions)

    for (i <- 0 to nSimulations-1) {
      println(s"Simulation $i:")

      var ithLearner = learner
      
      for (j <- 0 to nInterventions-1) {
        // https://stackoverflow.com/questions/3645045/in-scala-or-java-how-to-print-a-line-to-console-replacing-its-previous-content
        printf("\r%2d", j)

        // stop if there is a priori no more to be learned, i.e., zero entropy
        if (ithLearner.hypsDist.entropy == 0.0) {
          // no more to be learned, so just append placeholders for keeping the length of all ithSim arrays the same (so that they can be smoothly injected into R)
          val stopEvent = Event(Set(Block("STOP")), false)
          sims(i)(j) = (stopEvent, 0.0, Map(), Map(), Map())

        } else {
          
          val intervention: Set[Block] = ithLearner.chooseIntervention()
          val event: Event = makeEvent(intervention)

          ithLearner = ithLearner.update(Vector(event))  // returns a new Learner object

          // the different posterior distributions
          val jointDist = ithLearner.hypsDist
          val fformDist = ithLearner.fformMarginal(jointDist)
          val structDist = ithLearner.structMarginal(jointDist)

          sims(i)(j) = (event, jointDist.entropy, jointDist.maxAtoms, fformDist.maxAtoms, structDist.maxAtoms)
        }
      }
    }

    sims
  }

  def makeEvent(combo: Set[Block]): Event = {
    // make a full Event (combo and outcome pair) from a combo wrt the true blickets and form

    val pPos: Double = trueForm.f(combo.intersect(trueBlickets).size)
    val boolOutcome: Boolean = RandUtils.sampleOutcome(pPos)
    Event(combo, boolOutcome)
  }
}
