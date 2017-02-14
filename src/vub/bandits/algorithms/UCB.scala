package vub.bandits.algorithms

import vub.bandits.{Bandit, utils}
import vub.bandits.algorithms.Report.{RunReport, StepReport}
import vub.bandits.rand.RNG

//TODO: normalized rewards!!!
object UCB {
  abstract class Bound {
    def >(b: Bound): Boolean
  }
  case class UpperBound(v: ValueFunction, upper: Double) extends Bound {
    def >(b: Bound): Boolean = {
      b match {
        case Maximizing(_) => false
        case UpperBound(_v, _upper) => v.value + upper > _v.value + _upper
      }
    }
  }
  case class Maximizing(v: ValueFunction) extends Bound {
    def >(b: Bound): Boolean = true
  }
  def boundedValue(c: Double, v: ValueFunction, n: Int): Bound = {
    if (v.k == 0) {
      //when k=0, this is considered a maximising action
      Maximizing(v)
    } else {
      val u = c*math.log(n.toDouble) / v.k.toDouble
      val b = math.sqrt(u)
      UpperBound(v, b)
    }
  }

  def bestArm(c: Double, values: Vector[ValueFunction], n: Int) = {
    var bestIndex = 0
    var bestBoundedValue = boundedValue(c, values(0), n)
    for (i <- 1 to (values.length - 1)) {
      val bv = boundedValue(c, values(i), n)
      if (bv > bestBoundedValue) {
        bestIndex = i
        bestBoundedValue = bv
      }
    }
    bestIndex
  }

  /**
    * Perform one iteration of the UCB algorithm:
    * - on a bandit
    * - operating on values (i.e Q_t's)
    * - at an iteration (0-based)
    * - using step sizes as produced by a stepSizeFunction
    * TODO: ref Sutton book
    */
  //TODO: bound iteration?
  def step(bandit: Bandit[Double],
           c: Double,
           values: Vector[ValueFunction],
           iteration: Int,
           rng: RNG): StepReport = {
    val arm = bestArm(c, values, iteration + 1)

    val reward = bandit.play(arm)
    values(arm).update(reward)

    new StepReport(arm, reward)
  }

  /**
    * Perform a number of steps (i.e. iterations) of the UCB algorithm:
    * - on a bandit
    * - operating on values (i.e Q_t's)
    * - using step size as produced by a stepSizeFunction
    * TODO: ref Sutton book
    */
  def run(bandit: Bandit[Double],
          c: Double,
          values: Vector[ValueFunction],
          steps: Int,
          rng: RNG): RunReport = {
    val stepResults = for {
      i <- Vector.range(0, steps)
      result = step(bandit, c, values, i, rng)
    } yield result
    new RunReport(stepResults)
  }
}
