package vub.bandits.algorithms

import vub.bandits.Bandit
import vub.bandits.algorithms.Report.{RunReport, StepReport}
import vub.bandits.rand.{Bernoulli, DiscreteUniform, RNG}

object EpsilonGreedy {
  private def bestArm(values: Vector[ValueFunction]): Int = {
    //TODO: performance?
    val v: ValueFunction = values.maxBy(_.value)
    values.indexOf(v)
  }

  /**
    * Perform a number of steps (i.e. iterations) of the epsilon-greedy algorithm:
    * - on a bandit
    * - with parameter epsilon
    * - operating on values (i.e Q_t's)
    * - using step size as produced by a stepSizeFunction
    * TODO: ref Sutton book
    */
  //TODO: bound epsilon
  def run(bandit: Bandit[Double],
          epsilon: Double,
          values: Vector[ValueFunction],
          steps: Int,
          rng: RNG): RunReport = {
    val stepResults = for {
      i <- Vector.range(0, steps)
      result = step(bandit, epsilon, values, rng)
    } yield result
    new RunReport(stepResults)
  }

  /**
    * Perform one iteration of the epsilon-greedy algorithm:
    * - on a bandit
    * - with parameter epsilon
    * - operating on values (i.e Q_t's)
    * - using step size as produced by a stepSizeFunction
    * TODO: ref Sutton book
    */
  //TODO: bound epsilon
  def step(bandit: Bandit[Double],
           epsilon: Double,
           values: Vector[ValueFunction],
           rng: RNG): StepReport = {
    //Bernoulli experiment:
    //- success = picking the best arm (greedy behaviour)
    //- failure = means picking a random arm other than the best arm
    val greedy = new Bernoulli(p = 1 - epsilon).sample(rng)

    val greedyArm = bestArm(values)
    val arm =
      if (greedy) {
        greedyArm
      } else {
        val arms = Vector.range(start = 0, end = bandit.nrArms)
        //randomly choose an arm that is NOT the greedy arm
        //filter out the best arm
        val choices = arms.filter(_ != greedyArm)
        //choose arm
        val r = new DiscreteUniform(low = 0, high = choices.length-1).sample(rng)
        val choice = choices(r)
        choice
      }

    val reward = bandit.play(arm)
    values(arm).update(reward)

    new StepReport(arm, reward)
  }
}
