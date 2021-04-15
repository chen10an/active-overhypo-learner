import activeOverhypoLearner._
import scala.util.Random

// simulation of model strategy wrt simple disj/conj space

// hyperparams
val totalInterventions = 10

// prepare inputs to the constructor
val conj = Fform("conj", (n: Int) => if(n >= 2) 1.0 else 0.0)
val disj = Fform("disj", (n: Int) => if(n >= 1) 1.0 else 0.0)
val priorPDisj = 0.9
val priorPConj = 1-priorPDisj
val fformDist = Dist(Map(disj -> priorPDisj, conj -> priorPConj))

val allBlocks = Set("0", "1", "2").map(Block(_))
val allCombos = allBlocks.subsets().toIndexedSeq.map(_.asInstanceOf[Set[Block]])

val uniformStructPrior = Dist[Set[Block]](allCombos.map(blickets => (blickets, 1.0)).toMap)
val p1Learner = PhaseLearner(fformDist, uniformStructPrior, allBlocks)
var currentLearner = p1Learner

// https://alvinalexander.com/scala/get-random-element-from-list-of-elements-scala/
def getRandomElement[A](seq: Seq[A], random: Random): A = 
    seq(random.nextInt(seq.length))

def makeEvent(combo: Set[Block], trueBlickets: Set[Block], trueForm: Fform): Event = {
  // make a full Event (combo and outcome pair) from a combo wrt the true blickets and form
  val outcome = trueForm.f(combo.intersect(trueBlickets).size)
  val boolOutcome: Boolean = outcome == 1.0  // TODO: probably need to make this more robust and to generalize it to nondeterministic forms
  Event(combo, boolOutcome)
}

var eventSim = Vector.empty[(Event, Double)]
for (i <- 1 to totalInterventions) {
  val bestCombos = currentLearner.comboRanks.filter(_._2 == 1).keys.toArray
  val sampleBestCombo = getRandomElement(bestCombos, new Random)
  val event = makeEvent(sampleBestCombo, Set("0", "1").map(Block(_)), conj)
  currentLearner = currentLearner.update(Vector(event))
  eventSim = eventSim :+ (event, currentLearner.hypsDist.entropy)
}

eventSim

println(eventSim)

