package vub.bandits.experiments.influenza

import java.io.File

import vub.bandits.rand.{DiscreteUniform, RNG}
import vub.bandits.{Action, Bandit}

import sys.process._

object VaccineArms {
  def play(arm: List[Char], workDir: File, fluteScript: File, rng: RNG) : Double = {
    val seed = DiscreteUniform(high = Short.MaxValue, low = 0).sample(rng)
    val attackRate = (fluteScript + " " + workDir + " " + seed + " " + arm.mkString(",") !!).toDouble
    val reward = 1 - attackRate
    reward
  }

  //create all possible combinations of 0,1 of length 5: [0,1]^5
  //do this by iterating from 0-32, converting each number to a 0-padded binary number of length 5
  def createArms() = {
    def intToPaddedBinary(digits: Int, i: Int): List[Char] =
    ("%0" + digits + "d").format(i.toBinaryString.toInt).toList
    val arms = List.range(0, 32).map(intToPaddedBinary(5, _))
    arms
  }

  def createBandit(arms: Seq[List[Char]], workDir: File, fluteScript: File, rng: RNG): Bandit[Double] = {
    def createAction(arm: List[Char]): Action[Double] = {
      new Action[Double](() => play(arm, workDir, fluteScript, rng))
    }
    new Bandit[Double](arms.map(createAction(_)))
  }
}
