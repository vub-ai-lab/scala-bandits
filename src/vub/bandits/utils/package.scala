package vub.bandits

package object utils {
  def normalize(x: Double, min: Double, max: Double): Double = {
    if (x < min) 0.0
    else if (x > max) 1.0
    else {
      val normalized = (x - min) / (max - min)
      normalized
    }
  }
}

