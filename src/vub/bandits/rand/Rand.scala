package vub.bandits.rand

trait Rand[T] {
  def sample(rng: RNG): T
}
