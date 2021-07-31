package mmb

import scala.collection.mutable.Buffer

/**
 * Mandelbrot set are the complex numbers c that do not escape
 * the circle of radius 2 in a squaring loop z = 0; z = z^2 + c.
 * http://en.wikipedia.org/wiki/Mandelbrot_set
 *
 * @param pixelWidth a width of pixel area to model
 * @param pixelHeight a height of pixel area to model
 * @param maxIteration a maximum number of iterations for a pixel
 */
class MandelbrotSet(val pixelWidth: Int, val pixelHeight: Int, val maxIteration: Int) {

  val palette = ColorTuple.palette(100, Array(
    (0, 300.0 / 360, 1.0, 0.0),
    (15, 250.0 / 360, 1.0, 0.5),
    (99, 300.0 / 360, 1.0, 1.0)))

  /**
   * Calculates color for each pixel in a given view.
   *
   * @param center a center point of a view in complex plane (real, imaginary)
   * @param width a width of a view in complex plane
   * @return running array of modeled pixels (red, green, blue)
   */
  def calculate(center: (Double, Double), width: Double) = {
    val pixels = Buffer.empty[Int]
    val d = width / this.pixelWidth
    var y = center._2 - (this.pixelHeight / 2) * d
    for (yi <- 0 until this.pixelHeight) {
      var x = center._1 - width / 2
      for (xi <- 0 until this.pixelWidth) {
        pixels += this.escapeIteration(x, y)
        x += d
      }
      y += d
    }
    pixels.toArray.map(
        i => this.palette((i.toDouble / this.maxIteration * (this.palette.size - 1)).toInt))
  }

  /**
   * Given complex numbers c = x + yi and z = 0,
   * tests how many iterations z = z^2 + c stays inside a circle of radius 2.
   * Optimizations are made to remove time consuming multiplications.
   */
  def escapeIteration(x: Double, y: Double) = {
    var iteration = 0
    var r = 0.0
    var i = 0.0
    var rsqr = 0.0
    var isqr = 0.0
    def square(value: Double) = value * value
    while (iteration < this.maxIteration && rsqr + isqr < 4) {
      i = square(r + i) - rsqr - isqr + y
      r = rsqr - isqr + x
      rsqr = square(r)
      isqr = square(i)
      iteration += 1
    }
    iteration
  }

}
