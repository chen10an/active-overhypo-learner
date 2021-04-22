package sim

import utils._
import learner._
import scala.util.Random

case class Simulator(learner: Learner, trueBlickets: Set[Block], trueForm: Fform) {
  def run(nInterventions: Int): Vector[(Event, Double)] = {
    var currentLearner = learner

    var eventSim = Vector.empty[(Event, Double)]
    for (i <- 1 to nInterventions) {
      // TODO: use softmax to sample intervention
      val bestCombos = currentLearner.comboRanks.filter(_._2 == 1).keys.toIndexedSeq
      val sampleBestCombo = getRandomElement(bestCombos, new Random)
      val event = makeEvent(sampleBestCombo)
      currentLearner = currentLearner.update(Vector(event))
      eventSim = eventSim :+ (event, BigDecimal(currentLearner.hypsDist.entropy).setScale(5, BigDecimal.RoundingMode.HALF_UP).toDouble)
    }

    eventSim
  }

  def makeEvent(combo: Set[Block]): Event = {
    // make a full Event (combo and outcome pair) from a combo wrt the true blickets and form
    val outcome = trueForm.f(combo.intersect(trueBlickets).size)
    val boolOutcome: Boolean = outcome == 1.0  // TODO: probably need to make this more robust and to generalize it to nondeterministic forms
    Event(combo, boolOutcome)
  }

  // https://alvinalexander.com/scala/get-random-element-from-list-of-elements-scala/
  def getRandomElement[A](seq: Seq[A], random: Random): A = 
      seq(random.nextInt(seq.length))

}
