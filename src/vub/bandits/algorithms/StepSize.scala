package vub.bandits.algorithms

/**
  * Trait that provides a StepSize function.
  * Reference: section 2.5 of the RL book (Sutton, 1998)
  */
trait StepSize {
  /**
    * Compute the StepSize based on the current step.
    */
  def stepSize(step: Int): Double
}

class ConstantStepSizeFunction (constant: Double) extends StepSize {
  def stepSize(step: Int) = constant
}

class HarmonicStepSizeFunction extends StepSize {
  def stepSize(step: Int) = 1.0 / (step + 1)
}
