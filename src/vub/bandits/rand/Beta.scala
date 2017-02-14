package vub.bandits.rand

class Beta(alpha: Double, beta: Double) extends Rand[Double] {
  override def sample(rng: RNG): Double = {
    val g = new breeze.stats.distributions.Beta(alpha, beta)(rng.rand)
    g.sample()
  }
}
