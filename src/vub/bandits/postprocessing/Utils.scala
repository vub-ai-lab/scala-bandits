package vub.bandits.postprocessing

object Utils {
  def cumulativeSum(v: Array[Double]) = {
    val cum = Array.fill(v.length)(0.0)
    cum(0) = v(0)
    for (i <- 1 to v.length - 1) {
      cum(i) = v(i) + cum(i - 1)
    }
    cum
  }
}
