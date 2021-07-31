package mmb

/**
 * Creates eased steps for value change so that the value
 * changes fast in the beginning and slowly in the end.
 *
 * @param begin a begin value
 * @param end an end value
 * @param steps a total number of steps for the change
 */
class Easing(val begin: Double, val end: Double, val steps: Int) {

  val change = end - begin
  val last = steps - 1

  /**
   * A changed value at the given step.
   */
  def apply(step: Int) = begin - (math.pow(step.toDouble / last - 1.0, 4) - 1.0) * change
}