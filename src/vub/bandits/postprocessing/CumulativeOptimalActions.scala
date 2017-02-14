package vub.bandits.postprocessing

import org.jfree.chart.ChartFactory
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import org.jfree.data.xy.DefaultXYDataset
import vub.bandits.algorithms.Report.RunReport

object CumulativeOptimalActions {
  def plot(bestArms: Vector[Int], experiments: Map[String, Vector[RunReport]], steps: Int) = {
    val x = Array.range(0, steps).map(_.toDouble)

    val dataset = new DefaultXYDataset
    for (algo <- experiments.keys) {
      val optimals = OptimalActions.percOptimal(bestArms, experiments(algo), steps)
      val y = Utils.cumulativeSum(optimals)
      dataset.addSeries(algo, Array(y,x))
    }

    val renderer = new XYLineAndShapeRenderer(true, false)

    val chart = ChartFactory.createScatterPlot(
      "Cumulative optimal actions",
      "Cumulative optimal actions (%)",
      "Iterations",
      dataset,
      org.jfree.chart.plot.PlotOrientation.HORIZONTAL,
      true,false,false
    )
    chart.getXYPlot.setRenderer(0, renderer)
    chart
  }
}
