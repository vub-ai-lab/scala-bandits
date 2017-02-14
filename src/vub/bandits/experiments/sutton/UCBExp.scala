package vub.bandits.experiments.sutton

import vub.bandits.algorithms.Report.RunReport
import vub.bandits.algorithms.{EpsilonGreedy, HarmonicStepSizeFunction, UCB, ValueFunction}
import vub.bandits.environments.SuttonTestbed
import vub.bandits.postprocessing._
import vub.bandits.rand.RNG

/**
  * Apply bandit-solving algorithms on the Sutton testbed.
  * Reference: RL book (Sutton, 1998) section 2.2
  */
object UCBExp extends App {
  def values: Vector[ValueFunction] =
    Vector.fill(nrArms)(new ValueFunction(stepSizeFn, 0))

  val rng = RNG.default(10)

  val nrArms = 10
  val nrBandits = 2000
  val steps = 1000

  val suttonBandits = Vector.fill(nrBandits)(SuttonTestbed.normalizedTestBandit(nrArms, rng))
  val bandits = suttonBandits.map(_.bandit)
  val bestArms = suttonBandits.map(_.bestArm)

  val stepSizeFn = new HarmonicStepSizeFunction

  val c = 2

  val experiments = Map[String, Vector[RunReport]] (
    "greedy epsilon=0.1" ->
      bandits.map(EpsilonGreedy.run(_, epsilon = 0.1, values = values, steps = 1000, rng)),
    "ucb" ->
      bandits.map(UCB.run(_, c, values = values, steps = steps, rng))
  )

  val title = "ucb"
  val avg = AverageReward.plot(experiments, steps)
  Plot.show(title, avg)
  val cum_avg = CumulativeAverageReward.plot(experiments, steps)
  Plot.show(title, cum_avg)
  val optimal = OptimalActions.plot(bestArms, experiments, steps)
  Plot.show(title, optimal)
  val cum_optimal = CumulativeOptimalActions.plot(bestArms, experiments, steps)
  Plot.show(title, cum_optimal)
}
