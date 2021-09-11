package utils

import scala.util.Random

case class Fform(name: String, f: Int => Double) {
  override def toString = name
// the function within each Fform object takes the number n of present blickets as input
}
case class Block(name: String) {override def toString=name}

case class Event(blocks: Set[Block], outcome: Boolean)
case class Hyp(blickets: Set[Block], fform: Fform) 
case class Dist[T](atoms: Map[T, Double], binSize: Double = 1.0) {
  // binSize is used for calculating the histogram approximation of entropy when the distribution is a binned, discrete approximation of a continuous distribution (e.g., a gamma distribution over continuous bias or gain values)
  // assuming binSize is constant throughout all atoms

  lazy val transformations = {
    // use one loop through the full hypothesis space to calculate all desired transformations

    // track desired transformations
    var entropy = 0.0
    var denom = 0.0  // for normalization
    var roundedMaxP: BigDecimal = 0.0  // highest value/probability
    var maxT = scala.collection.mutable.Set[T]()  // keys/hypotheses that have maxp

    // these are only updated when T is Hyp:
    var fformMarginalAtoms = collection.mutable.Map[Fform, Double]()
    var structMarginalAtoms = collection.mutable.Map[Set[Block], Double]()

    for ((t, p) <- atoms) {
      // round to allow precision errors in comparisons
      val roundedP = NumberUtils.round(p)
      assert(roundedP >= 0.0 && roundedP <= 1.0)

      // update the max probability and its associated keys/hypotheses
      if (roundedP > roundedMaxP) {
        roundedMaxP = roundedP
        maxT = scala.collection.mutable.Set(t)
      } else if (roundedP == roundedMaxP) {
        maxT += t
      }

      // calculate plogp (to be summed for entropy)
      if (p > 0.0) {
        entropy += p*log2(p/binSize)  // divide by binSize for histogram approximation, same as regular discrete entropy when binSize=1
      }

      denom += p

      // only update the marginal fform and struct distributions if the keys have the Hyp type
      t match {
        case hyp: Hyp =>
          // increment the relevant marginal probabilities
          fformMarginalAtoms(hyp.fform) = fformMarginalAtoms.getOrElseUpdate(hyp.fform, 0.0) + p
          structMarginalAtoms(hyp.blickets) = structMarginalAtoms.getOrElseUpdate(hyp.blickets, 0.0) + p
        case _ =>  // do nothing otherwise
      }
    }

    entropy = -entropy  // negate the sum

    // return all the desired transformations in one tuple
    (entropy, denom, roundedMaxP.toDouble, maxT, fformMarginalAtoms, structMarginalAtoms)
  }

  lazy val entropy: Double = {
    // -atoms.values.map(v => if(v == 0) 0.0 else v*log2(v)).sum
    transformations._1
  }

  lazy val normalize = {
    // val denom = atoms.values.sum
    val denom = transformations._2

    // return normalized copy with the same binSize
    if (denom == 0.0) {
      Dist(atoms.map(p => p._1 -> 0.0), binSize)  
    } else {
      Dist(atoms.map(p => p._1 -> p._2/denom), binSize)
    }
  }

  // return the highest probability density points in the same format as the input atoms
  lazy val maxAtoms: Map[T, Double] = transformations._4.map(t => (t, transformations._3)).toMap

  // these are only non-empty when T is Hyp; toMap turns them from mutable to immutable, which is the required type for initializing Dist
  lazy val fformMarginalAtoms: Map[Fform, Double] = transformations._5.toMap
  lazy val structMarginalAtoms = transformations._6.toMap

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

object TimeUtils {
  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0).toDouble/1000000.0 + "ms")
    result
  }
}
