import utils._
import learner._
import priors._
import sim._
import scala.util.Random

// simulation of model strategy wrt simple disj/conj space

// prior
val ss = SimpleSpace(0.9, Set("0", "1", "2").map(Block(_)), true)
ss

val p1Learner = PhaseLearner(ss.hypsDist)
p1Learner.fformMarginal(p1Learner.hypsDist).atoms.values

val simulator = Simulator(p1Learner, Set("0").map(Block(_)), ss.disj)
simulator.run(5)

// debug sim:
// val events = Vector(Event(Set("0").map(Block(_)), true), Event(Set("1").map(Block(_)), false))  //, Event(Set("0", "1").map(Block(_)), true))

// val x = p1Learner.update(events)
// x.fformMarginal(x.multiPosterior(Vector(events(0)))).atoms.values
// x
