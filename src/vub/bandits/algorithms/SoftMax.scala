package vub.bandits.algorithms

import vub.bandits.Bandit
import vub.bandits.algorithms.Report.{RunReport, StepReport}
import vub.bandits.rand.{Choice, RNG}

object SoftMax {
  private def selectionProbs(values: Vector[ValueFunction],
                            tau: Double): Vector[Double] = {
    def expValue(v: ValueFunction) =
      math.exp(v.value / tau)
    val exps = values.map(expValue(_))
    val norm = exps.sum
    exps.map(_ / norm)
  }

  /**
    * Perform one iteration of the softmax algorithm:
    * - on a bandit
    * - with parameter tau
    * - operating on values (i.e Q_t's)
    * - using step sizes as produced by a stepSizeFunction
    * TODO: ref Sutton book
    */
  //TODO: bound tau
  def step(bandit: Bandit[Double],
           tau: Double,
           values: Vector[ValueFunction],
           rng: RNG): StepReport = {
    val arms = Vector.range(0, bandit.nrArms)

    val arm = Choice.choose(arms, selectionProbs(values, tau), rng)

    val reward = bandit.play(arm)
    values(arm).update(reward)

    new StepReport(arm, reward)
  }

  /**
    * Perform a number of steps (i.e. iterations) of the epsilon-greedy algorithm:
    * - on a bandit
    * - with parameter tau
    * - operating on values (i.e Q_t's)
    * - using step size as produced by a stepSizeFunction
    * TODO: ref Sutton book
    */
  //TODO: bound tau
  def run(bandit: Bandit[Double],
          tau: Double,
          values: Vector[ValueFunction],
          steps: Int,
          rng: RNG): RunReport = {
    val stepResults = for {
      i <- Vector.range(0, steps)
      result = step(bandit, tau, values, rng)
    } yield result
    new RunReport(stepResults)
  }
}
