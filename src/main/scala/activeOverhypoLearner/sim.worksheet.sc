import utils._
import learner._
import priors._
import sim._

// simulation of model strategy wrt simple disj/conj space

// prior
val ss1 = SimpleSpace(0.9, Set("0", "1", "2").map(Block(_)), true)
val ss2 = SimpleSpace(0.9, Set("0", "1", "2", "3", "4", "5").map(Block(_)), true)
val ss3 = SimpleSpace(0.9, Set("0", "1", "2", "3", "4", "5", "6", "7", "8").map(Block(_)), true)

val p1Learner = PhaseLearner(ss1.hypsDist)
val p2Learner = PhaseLearner(ss2.hypsDist)
val p3Learner = PhaseLearner(ss3.hypsDist)

val d1sim = Simulator(p1Learner, Set("0").map(Block(_)), ss1.disj)
val c1sim = Simulator(p1Learner, Set("0", "1").map(Block(_)), ss1.conj)

val d2sim = Simulator(p2Learner, Set("0", "1", "2").map(Block(_)), ss2.disj)
val c2sim = Simulator(p2Learner, Set("0", "1", "2").map(Block(_)), ss2.conj)

val d3sim = Simulator(p3Learner, Set("0", "1", "2", "3").map(Block(_)), ss3.disj)
val c3sim = Simulator(p3Learner, Set("0", "1", "2", "3").map(Block(_)), ss3.conj)


// debug sim:
// val events = Vector(Event(Set("0").map(Block(_)), true), Event(Set("1").map(Block(_)), false))  //, Event(Set("0", "1").map(Block(_)), true))

// val x = p1Learner.update(events)
// x.fformMarginal(x.multiPosterior(Vector(events(0)))).atoms.values
// x
