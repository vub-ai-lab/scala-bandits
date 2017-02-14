package vub.bandits.algorithms

/**
  * Value class
  * Q_t in section 2.2 of the RL book (Sutton, 1998)
  */
class ValueFunction
  (val stepSizeFun: StepSize,
   private var _value: Double) {
  private var _k = 0
  def k = _k

  def value = this._value

  def copy(): ValueFunction = {
    val vf = new ValueFunction(stepSizeFun, value)
    vf._k = this._k
    vf
  }

  def update(reward: Double) = {
    val Q_k = value

    //Incremental update rule
    //Update rule 2.4 in section 2.5 of the RL book (Sutton, 1998)
    val stepSize = stepSizeFun.stepSize(_k)
    val Q_k_next = Q_k + stepSize * (reward - Q_k)

    _k = _k + 1
    _value = Q_k_next
  }
}
