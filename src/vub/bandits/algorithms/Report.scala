package vub.bandits.algorithms

object Report {
  class StepReport(val arm: Int,
                   val reward: Double)

  class RunReport(val steps: Vector[StepReport])
}

