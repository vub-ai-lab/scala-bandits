package vub.bandits.experiments.sutton

import java.io.BufferedWriter

import vub.bandits.algorithms.Report.RunReport
import vub.bandits.environments.SuttonTestbed.SuttonBandit

object Csv {
  def export[T](out: BufferedWriter,
                bandits: Vector[SuttonBandit],
                experiments: Map[String, Vector[RunReport]],
                steps: Int) = {
    //header
    out.append("bandit,optimal_arm,algorithm,iteration,reward,arm")
    out.append("\n")

    val algorithms = experiments.keys
    for (algorithm <- algorithms) {
      val experiment = experiments(algorithm)
      for (i <- 0 to bandits.length-1) {
        val bandit = bandits(i)
        val run = experiment(i)

        for (j <- 0 to steps-1) {
          val step = run.steps(j)
          out.append(i.toString)
          out.append(",")
          out.append(bandit.bestArm.toString)
          out.append(",")
          out.append(algorithm)
          out.append(",")
          out.append(j.toString)
          out.append(",")
          out.append(step.reward.toString)
          out.append(",")
          out.append(step.arm.toString)
          out.append("\n")
        }
      }
    }
  }
}
