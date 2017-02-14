package vub.bandits.rand

trait Dist {
  def pdf(x: Double): Double
  def cdf(x: Double): Double
}
