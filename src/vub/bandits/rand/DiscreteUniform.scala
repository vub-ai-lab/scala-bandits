package vub.bandits.rand

case class DiscreteUniform(low: Int, high: Int) extends Rand[Int] {
  def sample(rng: RNG): Int = rng.rand.randInt((high - low) + 1).sample() + low
}

