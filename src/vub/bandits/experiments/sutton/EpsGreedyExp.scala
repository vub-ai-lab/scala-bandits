package vub.bandits.experiments.sutton

import vub.bandits.algorithms.Report.RunReport
import vub.bandits.algorithms.{EpsilonGreedy, HarmonicStepSizeFunction, ValueFunction}
import vub.bandits.environments.SuttonTestbed
import vub.bandits.postprocessing._
import vub.bandits.rand.RNG

/**
  * Apply bandit-solving algorithms on the Sutton testbed.
  * Reference: RL book (Sutton, 1998) section 2.2
  */
object EpsGreedyExp extends App {
  def values: Vector[ValueFunction] =
    Vector.fill(nrArms)(new ValueFunction(stepSizeFn, 0))

  val rng = RNG.default(10)

  val nrArms = 10
  val nrBandits = 2000
  val steps = 1000

  val suttonBandits = Vector.fill(nrBandits)(SuttonTestbed.testBandit(nrArms, rng))
  val bandits = suttonBandits.map(_.bandit)
  val bestArms = suttonBandits.map(_.bestArm)

  val stepSizeFn = new HarmonicStepSizeFunction

  val experiments = Map[String, Vector[RunReport]] (
    "greedy epsilon=0.0" ->
      bandits.map(EpsilonGreedy.run(_, epsilon = 0.0, values = values, steps = 1000, rng)),
    "greedy epsilon=0.1" ->
      bandits.map(EpsilonGreedy.run(_, epsilon = 0.1, values = values, steps = 1000, rng)),
    "greedy epsilon=0.01" ->
      bandits.map(EpsilonGreedy.run(_, epsilon = 0.01, values = values, steps = 1000, rng))
  )

  val avg = AverageReward.plot(experiments, steps)
  Plot.show("eps-greedy", avg)
  val cum_avg = CumulativeAverageReward.plot(experiments, steps)
  Plot.show("eps-greedy", cum_avg)
  val optimal = OptimalActions.plot(bestArms, experiments, steps)
  Plot.show("eps-greedy", optimal)
  val cum_optimal = CumulativeOptimalActions.plot(bestArms, experiments, steps)
  Plot.show("eps-greedy", cum_optimal)
}
