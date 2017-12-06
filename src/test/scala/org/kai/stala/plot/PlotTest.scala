package org.kai.stala.plot

import java.awt.Color

import org.junit.Test

class PlotTest {
  val x = Range.Double(-5, 5, 0.25)
  val y1 = x.map(t => math.sin(t*10))
  val y2 = x.map(math.abs)

  @Test
  def SeriesPlotTest(): Unit ={
    val s1 = SeriesElement(y1, x,
      point = Plot.Point.CIRCLE_POINT(),
      line = Plot.Line.SOLID_LINE(color = Color.blue, style = Plot.Line.Style.DASH),
      name = "sin")
    val s2 = SeriesElement(y2, x, point = Plot.Point.EMPTY_CIRCLE_POINT(color = Color.red), name = "abs")
    val plot = SeriesPlot(Seq(s1, s2))
    plot.showInFrame
    Thread.sleep(5000)
  }

  @Test
  def SeriesPlotTest2(): Unit ={
    val s1 = SeriesElement(y1,
      point = Plot.Point.CIRCLE_POINT(),
      line = Plot.Line.SOLID_LINE(color = Color.blue, style = Plot.Line.Style.DASH),
      name = "sin")
    val s2 = SeriesElement(y2, point = Plot.Point.EMPTY_CIRCLE_POINT(color = Color.red), name = "abs")
    val plot = SeriesPlot(Seq(s1, s2))
    plot.showInFrame
    Thread.sleep(5000)
  }

  @Test
  def MultiPlotTest(): Unit ={
    val s1 = SeriesElement(x, y1,
      point = Plot.Point.CIRCLE_POINT(),
      line = Plot.Line.SOLID_LINE(color = Color.blue, style = Plot.Line.Style.DASH),
      name = "sin")
    val s2 = SeriesElement(x, y2, point = Plot.Point.EMPTY_CIRCLE_POINT(color = Color.red), name = "abs")
    val plot = SeriesPlot(Seq(s1, s2))
    val plot2 = plot.copy()
    val multiPlot = MultiPlot(Seq(plot, plot2), syncNavigator = true, title = Some("Combined Graph"))
    multiPlot.showInFrame
    Thread.sleep(5000)
  }
}
