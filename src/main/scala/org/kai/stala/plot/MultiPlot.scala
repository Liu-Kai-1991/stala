package org.kai.stala.plot

import de.erichseifert.gral.graphics.DrawableContainer
import de.erichseifert.gral.graphics.layout.TableLayout
import java.awt.Color

import de.erichseifert.gral.navigation.Navigable
import de.erichseifert.gral.plots.XYPlot

case class MultiPlot(
  plotElements: Seq[Plot],
  backGroundColor: Color = Color.WHITE,
  title: Option[String] = None,
  plotWidth: Int = 800,
  plotHeight: Int = 600,
  syncNavigator: Boolean = false
) extends Plot {
  init()
  val plot = new DrawableContainer(new TableLayout(1))
  plotElements.foreach{
    p => plot.add(p.plot)
  }
  if (syncNavigator){
    val navigables = plotElements.toList.collect{
      case p if p.plot.isInstanceOf[Navigable] =>
        p.plot.asInstanceOf[Navigable]
    }
    navigables match {
      case Nil =>
      case head::Nil =>
      case head::tail =>
        tail.foreach(a => a.getNavigator.connect(head.getNavigator))
    }
  }
}