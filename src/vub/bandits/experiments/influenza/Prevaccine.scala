package vub.bandits.experiments.influenza

import java.io.{File, PrintStream}

import vub.bandits.algorithms.Report.{RunReport, StepReport}
import vub.bandits.algorithms.{ConstantStepSizeFunction, EpsilonGreedy, UCB, ValueFunction}
import vub.bandits.rand.RNG

object Prevaccine extends App {
  if (getParam(args, "seed").isEmpty || getParam(args, "steps").isEmpty ||
        getParam(args, "flutescript").isEmpty || getParam(args, "workdir").isEmpty ||
        getParam(args, "algo").isEmpty) {
    print("--seed=$se --steps=$st --flutescript=$fs --workdir=$wd " +
            "--algo=[eps-greedy($eps), ucb] [--algo-output=algo-out.csv]")
    System.exit(1)
  }

  val rng = RNG.default(getParam(args, "seed").get.toInt)
  val steps = getParam(args, "steps").get.toInt

  val workDir = new File(getParam(args, "workdir").get)

  val fluteScript = new File(getParam(args, "flutescript").get)

  val algo = getParam(args, "algo").get
  val algoOutput: Option[File] = getParam(args, "algo-output") match {
    case None => None
    case Some(f) => Some(new File(f))
  }

  val arms = VaccineArms.createArms()
  val bandit = VaccineArms.createBandit(arms, workDir, fluteScript, rng)

  if (algo.startsWith("eps-greedy")) {
    val parens = "eps-greedy\\(([^)]+)\\)".r
    val parens(eps) = algo

    val values = Vector.fill(bandit.nrArms)(new ValueFunction(new ConstantStepSizeFunction(0.1), 1))

    val stepResults:Vector[(StepReport,Vector[ValueFunction])] = for {
      iteration <- Vector.range(0, steps)
      previousValues: Vector[ValueFunction] = values.map(_.copy())
      result = EpsilonGreedy.step(bandit, eps.toDouble, values, rng)
    } yield (result, previousValues)

    val (stepReports, valuesHistory) = stepResults.unzip

    if (algoOutput.isDefined)
      qValuesToCsv(new PrintStream(algoOutput.get), bandit, valuesHistory)

    toCsv(Console.out, "eps-greedy", arms, new RunReport(stepReports))
  } else if (algo.startsWith("ucb")) {
    val c = 0.001

    val values = Vector.fill(bandit.nrArms)(new ValueFunction(new ConstantStepSizeFunction(0.1), 1))

    val stepResults:Vector[(StepReport,Vector[ValueFunction])] = for {
      iteration <- Vector.range(0, steps)
      previousValues: Vector[ValueFunction] = values.map(_.copy())
      result = UCB.step(bandit, c, values, iteration, rng)

    } yield (result, previousValues)

    val (stepReports, valuesHistory) = stepResults.unzip

    toCsv(Console.out, "eps-greedy", arms, new RunReport(stepReports))

    if (algoOutput.isDefined)
      qValuesToCsv(new PrintStream(algoOutput.get), bandit, valuesHistory)
  } else {
    throw new IllegalArgumentException("Algorithm \"" + algo + "\" is not recognized")
  }
}
