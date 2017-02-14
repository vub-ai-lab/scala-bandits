package vub.bandits.environments

import vub.bandits.rand.{Gaussian, RNG}
import vub.bandits.{Action, Bandit, utils}

object SuttonTestbed {
  def Q_star(rng: RNG): Double = {
    Gaussian(mean = 0.0, sd = 1.0).sample(rng)
  }

  class SuttonBandit(val bandit: Bandit[Double], val bestArm: Int)

  /**
    * Create a test bandit (type: [[vub.bandits.Bandit]])
    * as described in section 2.2 of the RL book (Sutton, 1998).
    *
    * The Q* for each arm is sampled from a Gaussian(0,1),
    * the arm's reward is sampled from a Gaussian(Q*,1).
    */
  def testBandit(nrArms: Int, rng: RNG): SuttonBandit = {
    def createAction(Q_star: Double) =
      new Action(() => Gaussian(Q_star, 1).sample(rng))
    createBandit(nrArms, createAction, rng)
  }

  /**
    * Create a test bandit (type: [[vub.bandits.Bandit]])
    * as described in section 2.2 of the RL book (Sutton, 1998).
    *
    * The Q* for each arm is sampled from a Gaussian(0,1),
    * the arm's reward is sampled from a Gaussian(Q*,1).
    *
    * Rewards are normalized to [0,1].
    */
  //TODO: try to make this of type UnitReal
  def normalizedTestBandit(nrArms: Int, rng: RNG): SuttonBandit = {
    //Normalization procedure:
    //We know that Q* is sampled from Gaussian(0,1)
    //and rewards are sampled from Gaussian(Q*,1).
    //Therefore,
    //we take the 6*sigma of Gaussian(0,1),
    //resulting in the bounds for Q*: [-6, 6].
    //We take the 6*sigma of Gaussian(-6,1) and Gaussian(6,1)
    //resulting in the reward bound [-12,12].

    def normalize(x: Double) = utils.normalize(x, -12, 12)
    def createAction(Q_star: Double) =
      new Action(() => normalize(Gaussian(Q_star, 1).sample(rng)))
    createBandit(nrArms, createAction, rng)
  }

  private def createBandit[T](nrArms: Int,
                              createAction: (Double) => Action[Double],
                              rng: RNG): SuttonBandit = {
    val Q_stars = List.fill(nrArms)(Q_star(rng))
    val actions = Q_stars.map(createAction(_))

    val bandit = new Bandit[Double](actions)
    val bestArm = Q_stars.indexOf(Q_stars.max)

    new SuttonBandit(bandit, bestArm)
  }
}
