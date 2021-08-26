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
  lazy val entropy: Double = {
    -atoms.values.map(v => if(v == 0) 0.0 else v*log2(v)).sum
  }

  private def log2(x: Double): Double = {
    math.log(x)/math.log(2)
  }

  def normalize = {
    val denom = atoms.values.sum

    // return normalized copy
    if (denom == 0) {
      Dist(atoms.map(p => p._1 -> 0.0))  
    } else {
      Dist(atoms.map(p => p._1 -> p._2/denom))  
    }
  }
}

object NumberUtils {
  val precisionScale = 10  // num decimal place rounding via BigDecimal --> comparisons are tolerant of imprecise differences beyond this num decimal places
  val roundingMode = BigDecimal.RoundingMode.HALF_UP

  // round before performing Double comparisons to allow for precision errors in the far-right decimal places
  // adapted from https://stackoverflow.com/questions/11106886/scala-doubles-and-precision
  def round(x: Double): BigDecimal = BigDecimal(x).setScale(precisionScale, roundingMode)
}

object RandUtils {
  // TODO: implement softmax here

  val random = new Random(0)  // seed for reproducibility

  // https://alvinalexander.com/scala/get-random-element-from-list-of-elements-scala/
  def getRandomElement[A](seq: Seq[A]): A =
    seq(random.nextInt(seq.length))
}
