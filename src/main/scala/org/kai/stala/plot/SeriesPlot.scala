package org.kai.stala.plot

import java.awt.{BorderLayout, Color}

import scala.reflect.{ClassTag, classTag}
import de.erichseifert.gral.data.{DataSeries, DataTable}
import de.erichseifert.gral.plots.XYPlot
import de.erichseifert.gral.plots.lines.{DefaultLineRenderer2D, LineRenderer}
import de.erichseifert.gral.plots.points.{DefaultPointRenderer2D, PointRenderer}
import de.erichseifert.gral.ui.InteractivePanel
import de.erichseifert.gral.graphics.Insets2D
import org.kai.stala.util._

case class SeriesPlot(
  seriesElements: Seq[SeriesElement],
  backGroundColor: Color = Color.WHITE,
  title: Option[String] = None,
  plotWidth: Int = 800,
  plotHeight: Int = 600,
  legendVisible: Boolean = false
) extends Plot {
  init()
  val plot = new XYPlot(seriesElements.map(_.dataSeries) :_*)
  seriesElements.foreach{
    case SeriesElement(ds, line, point, name) =>
      plot.setPointRenderers(ds, point)
      plot.setLineRenderers(ds, line)
  }
  plot.setInsets(new Insets2D.Double(20.0, 40.0, 40.0, 40.0))
  plot.setLegendVisible(legendVisible)
}

case class SeriesElement private(
  dataSeries: DataSeries,
  line: LineRenderer,
  point: PointRenderer,
  name: String
)

object SeriesElement{

  def apply[XT: ClassTag, YT: ClassTag](
    ys: Iterable[YT],
    xs: Iterable[XT] = Iterable(),
    line: LineRenderer = new DefaultLineRenderer2D,
    point: PointRenderer = new DefaultPointRenderer2D,
    name: String = ""
  ): SeriesElement = {
    if (xs.isEmpty)
      apply(ys, Range(0, ys.size), line, point, name)
    else {
      val xClass = javaClass(classTag[XT].runtimeClass)
      val yClass = javaClass(classTag[YT].runtimeClass)
      val dataTable = new DataTable(xClass, yClass)
      (toJavaType(xs), toJavaType(ys)).zipped.foreach{
        case (x,y) => dataTable.add(x, y)
      }
      val dataSeries = new DataSeries(name, dataTable, 0, 1)
      new SeriesElement(dataSeries, line, point, name)
    }
  }
}
