package vub.bandits.postprocessing

import org.jfree.chart.ChartFactory
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import org.jfree.data.xy.DefaultXYDataset
import vub.bandits.algorithms.Report.RunReport

object AverageReward {
  def averageReward(experiment: Vector[RunReport], steps: Int): Array[Double] = {
    val averages = Array.fill(steps)(0.0)
    for (i <- 0 to steps - 1) {

      for (j <- 0 to experiment.length - 1) {
        //println (i+","+j)
        averages(i) += experiment(j).steps(i).reward
      }
    }
    for (i <- 0 to steps - 1) {
      averages(i) /= experiment.length
    }
    averages
  }

  def plot(experiments: Map[String, Vector[RunReport]], steps: Int) = {
    val x = Array.range(0, steps).map(_.toDouble)

    val dataset = new DefaultXYDataset
    for (algo <- experiments.keys) {
      val y = averageReward(experiments(algo), steps)
      dataset.addSeries(algo, Array(y,x))
    }

    val renderer = new XYLineAndShapeRenderer(true, false)

    val chart = ChartFactory.createScatterPlot(
        "Average reward",
        "Average reward",
        "Iterations",
        dataset,
        org.jfree.chart.plot.PlotOrientation.HORIZONTAL,
        true,false,false
      )
    chart.getXYPlot.setRenderer(0, renderer)
    chart
  }
}
