package vub.bandits

/**
  * A n-armed bandit,
  * the bandit's n arms are represented by n actions (type: [[Action]]).
  * Reference: RL book (Sutton, 1998), section 2.1
  */
class Bandit[T] (actions: Seq[Action[T]]) {
  private val _actions = actions.toVector

  def play(armIndex: Int): T =
    _actions(armIndex).play()

  def nrArms: Int = _actions.length
}
