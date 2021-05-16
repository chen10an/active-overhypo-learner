package learner

import utils._
import scala.util.Random
import util.control.Breaks._

case class Simulator(learner: Learner, trueBlickets: Set[Block], trueForm: Fform) {
  val random = new Random(0)

  def run(nSimulations: Int, nInterventions: Int): Array[Array[(Event, Double)]] = {

    var sims = Array.empty[Array[(Event, Double)]]

    for (i <- 1 to nSimulations) {
      println(s"Simulation $i:")

      var ithLearner = learner
      var ithSim = Array.empty[(Event, Double)]

      var entropy = NumberUtils.round(ithLearner.hypsDist.entropy).toDouble
      
      for (j <- 1 to nInterventions) {
        // https://stackoverflow.com/questions/3645045/in-scala-or-java-how-to-print-a-line-to-console-replacing-its-previous-content
        printf("\r%2d", j)
        if (entropy == 0.0) {
          // no more to be learned, so just append placeholders for keeping the length of all ithSim arrays the same (so that they can be smoothly injected into R)
          val stopEvent = Event(Set(Block("STOP")), false)
          val entropy = 0.0
          ithSim = ithSim :+ (stopEvent, entropy)

        } else {
          // TODO: use softmax to sample intervention
          val bestCombos = ithLearner.comboRanks.filter(_._2 == 1).keys.toIndexedSeq
          val sampleBestCombo = getRandomElement(bestCombos)
          val event = makeEvent(sampleBestCombo)

          ithLearner = ithLearner.update(Vector(event))
          entropy = NumberUtils.round(ithLearner.hypsDist.entropy).toDouble

          ithSim = ithSim :+ (event, entropy)
        }
      }

      sims = sims :+ ithSim
    }

    sims
  }

  def makeEvent(combo: Set[Block]): Event = {
    // make a full Event (combo and outcome pair) from a combo wrt the true blickets and form
    val outcome = trueForm.f(combo.intersect(trueBlickets).size)
    val boolOutcome: Boolean = outcome == 1.0  // TODO: probably need to make this more robust and to generalize it to nondeterministic forms
    Event(combo, boolOutcome)
  }

  // https://alvinalexander.com/scala/get-random-element-from-list-of-elements-scala/
  def getRandomElement[A](seq: Seq[A]): A = 
      seq(random.nextInt(seq.length))

}
