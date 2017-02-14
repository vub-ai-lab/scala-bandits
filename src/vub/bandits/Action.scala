package vub.bandits

/**
  * An action returns a reward when played.
  * Reference: RL book (Sutton, 1998), section 2.1
  */
//TODO: maybe make a trait out of this?
class Action[T] (_play: () => T) {
  def play(): T = _play()
}
