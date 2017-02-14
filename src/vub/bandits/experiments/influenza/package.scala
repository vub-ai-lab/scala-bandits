package vub.bandits.experiments

import java.io.PrintStream

import vub.bandits.{Action, Bandit}
import vub.bandits.algorithms.Report.RunReport
import vub.bandits.algorithms.ValueFunction
import vub.bandits.experiments.influenza.Prevaccine._
import vub.bandits.rand.{DiscreteUniform, RNG}

package object influenza {
  def getParam(args: Array[String], name: String): Option[String] = {
    val needle = args.filter(_.startsWith("--" + name + "="))
    if (needle.length == 1)
      Some(needle(0).split("=")(1))
    else if (needle.length == 0)
      None
    else
      throw new IllegalArgumentException("More than 1 argument named \"" + name + "\"")
  }

  def qValuesToCsv[T](out: PrintStream, bandit: Bandit[T], values: Vector[Vector[ValueFunction]]) = {
    //header
    out.append("step,arm,q-value" + "\n")

    for {
      step <- 0 to values.length - 1
      arm <- 0 to bandit.nrArms - 1
    } out.append(step + "," + arm + "," + values(step)(arm).value + "\n")
  }

  def toCsv(out: PrintStream, algorithm: String, arms: List[List[Char]], report: RunReport) = {
    //header
    out.append("algorithm")
    for (i <- Vector.range(0, steps)) {
      out.append(",arm_" + i + ",reward" + i)
    }
    out.append("\n")

    out.append(algorithm)
    for (i <- Vector.range(0, steps)) {
      val step = report.steps(i)
      val arm = arms(step.arm).mkString
      out.append("," + arm + "," + step.reward)
    }
  }
}
