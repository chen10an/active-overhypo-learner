package utils

import scala.util.Random

case class Fform(name: String, f: Int => Double) {
  override def toString = name
// the function within each Fform object takes the number n of present blickets as input
}
case class Block(name: String) {override def toString=name}

case class Event(blocks: Set[Block], outcome: Boolean)
case class Hyp(blickets: Set[Block], fform: Fform) 
case class Dist[T](atoms: Map[T, Double]) {

  lazy val transformations = {
    // use one loop through the full hypothesis space to calculate all desired transformations

    // track desired transformations
    var entropy = 0.0
    var denom = 0.0  // for normalization
    var roundedMaxP: BigDecimal = 0.0  // highest value/probability
    var maxT = scala.collection.mutable.Set[T]()  // keys/hypotheses that have maxp
    for ((t, p) <- atoms) {
      // 3 d.p. rounding allowance for the assertion
      val assertionP = NumberUtils.round(p, 3)
      assert(assertionP >= 0.0 && assertionP <= 1.0)

      // round to allow precision errors in comparisons
      val roundedP = NumberUtils.round(p)

      // update the max probability and its associated keys/hypotheses
      if (roundedP > roundedMaxP) {
        roundedMaxP = roundedP
        maxT = scala.collection.mutable.Set(t)
      } else if (roundedP == roundedMaxP) {
        maxT += t
      }

      // calculate plogp (to be summed for entropy)
      if (p != 0.0) {
        entropy += p*log2(p)
      }

      denom += p
    }

    entropy = -entropy  // negate the sum
    denom = NumberUtils.round(denom).toDouble  // in case the denominator is basically zero, barring precision errors

    // return all the desired transformations in one tuple
    (entropy, denom, roundedMaxP.toDouble, maxT)
  }

  lazy val entropy: Double = {
    // -atoms.values.map(v => if(v == 0) 0.0 else v*log2(v)).sum
    transformations._1
  }

  lazy val normalize = {
    // val denom = atoms.values.sum
    val denom = transformations._2

    // return normalized copy
    if (denom == 0.0) {
      Dist(atoms.map(p => p._1 -> 0.0))  
    } else {
      Dist(atoms.map(p => p._1 -> p._2/denom))  
    }
  }

  // return the highest probability density points in the same format as the input atoms
  lazy val maxAtoms: Map[T, Double] = transformations._4.map(t => (t, transformations._3)).toMap

  private def log2(x: Double): Double = {
    math.log(x)/math.log(2)
  }
}

object NumberUtils {
  val precisionScale: Int = 10  // num decimal place rounding via BigDecimal --> comparisons are tolerant of imprecise differences beyond this num decimal places
  val roundingMode = BigDecimal.RoundingMode.HALF_UP

  // round before performing Double comparisons to allow for precision errors in the far-right decimal places
  // adapted from https://stackoverflow.com/questions/11106886/scala-doubles-and-precision
  def round(x: Double, precision: Int = precisionScale): BigDecimal = BigDecimal(x).setScale(precision, roundingMode)
}

object RandUtils {
  // TODO: implement softmax here

  val random = new Random(0)  // seed for reproducibility

  // https://alvinalexander.com/scala/get-random-element-from-list-of-elements-scala/
  def sampleRandomElement[A](seq: Seq[A]): A = seq(random.nextInt(seq.length))

  // sample a positive outcome (true) with probability p
  def sampleOutcome(p: Double): Boolean = random.nextDouble() < p
}
