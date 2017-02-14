package vub.bandits.postprocessing

import org.jfree.chart.{ChartFrame, JFreeChart}

object Plot {
  def show(title: String, chart: JFreeChart) = {
    val frame = new ChartFrame(title, chart)
    frame.pack()
    frame.setVisible(true)
  }
}
