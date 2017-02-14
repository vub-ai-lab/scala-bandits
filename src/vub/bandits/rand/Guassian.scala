package vub.bandits.rand

import breeze.numerics._

case class Gaussian(mean: Double, sd: Double) extends Dist with Rand[Double] {
  override def sample(rng: RNG): Double = {
    val g = breeze.stats.distributions.Gaussian(mu=mean, sigma=sd)(rng.rand)
    g.sample()
  }

  override def pdf(x: Double): Double = {
    //TODO: for now, use the Breeze library, but find a more elegant solution
    val g = breeze.stats.distributions.Gaussian(mu=mean, sigma=sd)
    g.pdf(x)
  }

  override def cdf(x: Double): Double = {
    .5 * (1 + erf( (x - mean)/ (Gaussian.sqrt2 * sd)))
  }
}

object Gaussian {
  private val sqrt2 = math.sqrt(2.0)
}
