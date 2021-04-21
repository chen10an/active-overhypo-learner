package utils

object NumberUtils {
  val precisionScale = 10  // num decimal place rounding via BigDecimal --> comparisons are tolerant of imprecise differences beyond this num decimal places
  val roundingMode = BigDecimal.RoundingMode.HALF_UP

  // round before performing Double comparisons to allow for precision errors in the far-right decimal places
  // adapted from https://stackoverflow.com/questions/11106886/scala-doubles-and-precision
  def round(x: Double): BigDecimal = BigDecimal(x).setScale(precisionScale, roundingMode)
}
