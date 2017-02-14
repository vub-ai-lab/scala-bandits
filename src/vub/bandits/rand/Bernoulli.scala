package vub.bandits.rand

class Bernoulli(p: Double) extends Rand[Boolean] {
  def sample(rng: RNG): Boolean = {
    val b = new breeze.stats.distributions.Bernoulli(p, rng.rand)
    b.sample()
  }
}
