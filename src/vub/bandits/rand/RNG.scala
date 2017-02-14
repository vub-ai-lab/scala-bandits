package vub.bandits.rand

import breeze.stats.distributions.{RandBasis, ThreadLocalRandomGenerator}
import org.apache.commons.math3.random.MersenneTwister

object RNG {
  def default(seed: Int): RNG = new RNG(new RandBasis(new ThreadLocalRandomGenerator(new MersenneTwister(seed))))
}

class RNG (private[rand] val rand: RandBasis)
