package vub.bandits.postprocessing

import java.awt.{BasicStroke, Color}

import org.jfree.chart.ChartFactory
import org.jfree.chart.axis.NumberAxis
import org.jfree.chart.renderer.xy.DeviationRenderer
import org.jfree.data.xy.{YIntervalSeries, YIntervalSeriesCollection}
import org.jfree.ui.RectangleInsets
import vub.bandits.Bandit
import vub.bandits.algorithms.UCB.{Maximizing, UpperBound}
import vub.bandits.algorithms.{UCB, ValueFunction}
import vub.bandits.rand.RNG

object UCB_QValues {
  def runAndPlot(values: Vector[ValueFunction], c: Double, b: Bandit[Double], steps: Int, rng: RNG) = {
    val series = for (i <- 0 to b.nrArms - 1) yield new YIntervalSeries("Q_" + i)

    val Q = values
    for (x <- 0 to steps - 1) {
      for (j <- 0 to b.nrArms - 1) {
        val y = Q(j).value
        val bound = UCB.boundedValue(c, Q(j), x + 1)
        val upper = bound match {
          case Maximizing(_) => 0
          case UpperBound(_v, _upper) => _upper
        }
        series(j).add(x, y, y, y + upper)
      }

      UCB.step(b, c, Q, x, rng)
      println("step:" + x)
    }

    val dataset = new YIntervalSeriesCollection()
    series.foreach {
      dataset.addSeries(_)
    }

    val chart = ChartFactory.createTimeSeriesChart(
      "Q-values", // chart title
      "Step", // x axis label
      "Q-value", // y axis label
      dataset, // data
      true, // include legend
      true, // tooltips
      false // urls
    );

    // get a reference to the plot for further customisation...
    val plot = chart.getXYPlot();
    plot.setDomainPannable(true);
    plot.setRangePannable(false);
    plot.setInsets(new RectangleInsets(5, 5, 5, 20));

    val renderer = new DeviationRenderer(true, false);
    renderer.setSeriesStroke(0, new BasicStroke(3.0f, BasicStroke.CAP_ROUND,
      BasicStroke.JOIN_ROUND));
    renderer.setSeriesStroke(0, new BasicStroke(3.0f,
      BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
    renderer.setSeriesStroke(1, new BasicStroke(3.0f,
      BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
    renderer.setSeriesFillPaint(0, new Color(255, 200, 200));
    renderer.setSeriesFillPaint(1, new Color(200, 200, 255));
    plot.setRenderer(renderer);

    // change the auto tick unit selection to integer units only...
    val yAxis = (plot.getRangeAxis());
    //yAxis.setAutoRangeIncludesZero(false);
    yAxis.setStandardTickUnits(NumberAxis.createIntegerTickUnits());

    Plot.show("Q-values", chart)
  }
}
