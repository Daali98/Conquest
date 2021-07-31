package mmb

import scala.math.max
import scala.math.min
import scala.collection.mutable.Buffer

/**
 * Provides methods to handle colors as tuples.
 *
 * RGB refers to a tuple (red, green, blue) with ranges 1-255
 * HSL refers to a tuple (hue, saturation, lightness) with ranges 0.0-1.0
 */
object ColorTuple {

  /**
   * Converts HSL color tuple to RGB color tuple.
   * http://www.niwa.nu/2013/05/math-behind-colorspace-conversions-rgb-hsl/
   */
  def HSLtoRGB(hsl: (Double, Double, Double)) = {
    def Int255(amount: Double) = (max(0.0, min(1.0, amount)) * 255).toInt
    def RGB255(rgb: (Double, Double, Double)) = (Int255(rgb._1), Int255(rgb._2), Int255(rgb._3))
    if (hsl._2 == 0.0) {
      RGB255((hsl._3, hsl._3, hsl._3))
    }
    val t1 = if (hsl._3 < 0.5) hsl._3 * (1.0 + hsl._2) else hsl._3 + hsl._2 - hsl._3 * hsl._2
    val t2 = 2.0 * hsl._3 - t1
    val t12 = (t1 - t2) * 6.0
    val tR = (hsl._1 + 0.333) % 1.0
    val tG = hsl._1
    val tB = if (hsl._1 < 0.333) hsl._1 - 0.333 + 1.0 else hsl._1 - 0.333
    def chTest(t1: Double, t2: Double, t12: Double, tC: Double) =
      if (6.0 * tC < 1.0) t2 + t12 * tC
      else if (2.0 * tC < 1.0) t1
      else if (3.0 * tC < 2.0) t2 + t12 * (0.666 - tC)
      else t2
    RGB255((chTest(t1, t2, t12, tR), chTest(t1, t2, t12, tG), chTest(t1, t2, t12, tB)))
  }

  /**
   * Creates an indexed color palette by sliding colors between the fixed color points.
   *
   * @param size the size of a palette
   * @param points the fixed colors (distance, hue, saturation, lightness)
   * @return array of colors (red, green, blue)
   */
  def palette(size: Int, points: IndexedSeq[(Int, Double, Double, Double)]) = {

    def DHSLtoIndex(size: Int, dhsl: (Int, Double, Double, Double)) = dhsl._1
    def DHSLtoRGB(dhsl: (Int, Double, Double, Double)) = HSLtoRGB((dhsl._2, dhsl._3, dhsl._4))

    val palette = Buffer.empty[(Int, Int, Int)]

    // Fill with first color until the point is reached.
    for (j <- 0 until DHSLtoIndex(size, points.head)) {
      palette += DHSLtoRGB(points.head)
    }

    // Travel each color point.
    for (i <- 0 until points.size - 1) {
      if (i == 0 || DHSLtoIndex(size, points(i)) > DHSLtoIndex(size, points(i - 1))) {
        palette += DHSLtoRGB(points(i))
      }

      // Fill the distance to next point with linearly interpolated colors.
      val d = DHSLtoIndex(size, points(i + 1)) - DHSLtoIndex(size, points(i))
      val h0 = points(i)._2
      val s0 = points(i)._3
      val l0 = points(i)._4
      val dH = (points(i + 1)._2 - h0) / d
      val dS = (points(i + 1)._3 - s0) / d
      val dL = (points(i + 1)._4 - l0) / d
      for (j <- 1 until d) {
        palette += HSLtoRGB((h0 + j * dH, s0 + j * dS, l0 + j * dL))
      }
    }

    // Fill with last color until the palette size is full.
    for (j <- DHSLtoIndex(size, points.last) until size) {
      palette += DHSLtoRGB(points.last)
    }
    palette.toArray
  }
}