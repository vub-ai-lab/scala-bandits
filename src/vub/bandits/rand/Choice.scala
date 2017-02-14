package vub.bandits.rand

object Choice {
  def choose[A](choices: Seq[A], probabilities: Seq[Double], rng: RNG): A = {
    val p = rng.rand.uniform.sample()
    val it = choices.zip(probabilities).iterator
    var accum = 0.0
    while (it.hasNext) {
      val (item, itemProb) = it.next
      accum += itemProb
      if (accum >= p)
        //return so that we don't have to search through the whole distribution
        return item
    }
    // needed so it will compile
    sys.error("this should never happen" + " p:" + p + " accum:" +accum)
  }
}