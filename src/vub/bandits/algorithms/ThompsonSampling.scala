package vub.bandits.algorithms

import vub.bandits.Bandit
import vub.bandits.algorithms.Report.{RunReport, StepReport}
import vub.bandits.rand.{Bernoulli, Beta, RNG}

//TODO: normalized rewards!!!
object ThompsonSampling {
  def theta(SI: (Int, Int), rng: RNG) = {
    val (s,i) = SI
    new Beta(s + 1, i + 1).sample(rng)
  }

  def bestArm(SIs: (Vector[(Int, Int)]), rng: RNG) = {
    var bestIndex = 0
    var bestTheta = theta(SIs(0), rng)
    for (i <- 1 to (SIs.length - 1)) {
      val t = theta(SIs(i), rng)
      if (t > bestTheta) {
        bestIndex = i
        bestTheta = bestTheta
      }
    }
    bestIndex
  }

  /**
    * Perform one iteration of the Thompson-Sampling algorithm:
    * - on a bandit
    * - operating on SI pair vector
    *     (parameter vectors for the Beta distribution)
    * - at an iteration (0-based)
    * - using step sizes as produced by a stepSizeFunction
    * TODO: add ref (Agrawal, 2012 paper)
    */
  //TODO: bound iteration?
  def step(bandit: Bandit[Double],
           SIs: (Vector[(Int, Int)]),
           iteration: Int,
           rng: RNG): StepReport = {
    val arm = bestArm(SIs, rng)

    val reward = bandit.play(arm)

    val b = new Bernoulli(p = reward).sample(rng)
    var (s,i) = SIs(arm)
    if (b) s += 1 else i += 1

    new StepReport(arm, reward)
  }

  /**
    * Perform a number of steps (i.e. iterations) of the Thompson-Sampling algorithm:
    * - on a bandit
    * - operating on values (i.e Q_t's)
    * - using step size as produced by a stepSizeFunction
    * TODO: ref Sutton book
    */
  def run(bandit: Bandit[Double],
          steps: Int,
          rng: RNG): RunReport = {
    val SIs = Vector.fill(bandit.nrArms)((0,0))
    val stepResults = for {
      i <- Vector.range(0, steps)
      result = step(bandit, SIs, i, rng)
    } yield result
    new RunReport(stepResults)
  }
}
