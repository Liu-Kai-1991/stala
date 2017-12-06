package org.kai.stala.plot

import java.awt.{BasicStroke, BorderLayout, Color, Dimension}
import javax.swing.{JFrame, JPanel}

import de.erichseifert.gral.plots.lines.{DefaultLineRenderer2D, DiscreteLineRenderer2D, LineRenderer, SmoothLineRenderer2D}
import de.erichseifert.gral.plots.points.{DefaultPointRenderer2D, PointRenderer}
import java.awt.geom.{Area, Ellipse2D}

import de.erichseifert.gral.graphics.DrawableContainer
import de.erichseifert.gral.ui.InteractivePanel

abstract class Plot extends JPanel(new BorderLayout){
  def plotWidth: Int
  def plotHeight: Int
  def backGroundColor: Color
  def title: Option[String]
  def init(): Unit = {
    setPreferredSize(new Dimension(plotWidth, plotHeight))
    setBackground(backGroundColor)
  }
  def plot: DrawableContainer
  def showInFrame: JFrame = {
    add(new InteractivePanel(plot), BorderLayout.CENTER)
    val frame = title match {
      case None => new JFrame()
      case Some(text) => new JFrame(text)
    }
    frame.getContentPane.add(this, BorderLayout.CENTER)
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.setSize(getPreferredSize)
    frame.setVisible(true)
    frame
  }
}

object Plot{
  object Line{
    object Style{
      val BASIC = new BasicStroke()
      val DASH = new BasicStroke(2.0f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER, 10.0f, Array[Float](3f, 3f), 0.0f)
    }
    def SOLID_LINE(color: Color = Color.BLACK, style: BasicStroke = Style.BASIC): LineRenderer = {
      val lineRenderer = new DefaultLineRenderer2D
      lineRenderer.setColor(color)
      lineRenderer.setStroke(style)
      lineRenderer
    }
    def ZIG_LINE(color: Color = Color.BLACK): LineRenderer = {
      val lineRenderer = new DiscreteLineRenderer2D
      lineRenderer.setColor(color)
      lineRenderer
    }
    def SMOOTH_LINE(color: Color = Color.BLACK): LineRenderer = {
      val lineRenderer = new SmoothLineRenderer2D
      lineRenderer.setColor(color)
      lineRenderer
    }
  }
  object Point{
    def None: PointRenderer = null
    def SQUARE_POINT(size: Double = 5.0, color: Color = Color.BLACK): PointRenderer = {
      val pointRenderer = new DefaultPointRenderer2D
      pointRenderer.setColor(color)
      pointRenderer
    }
    def CIRCLE_POINT(size: Double = 5.0, color: Color = Color.BLACK): PointRenderer = {
      val pointRenderer = new DefaultPointRenderer2D
      val circle = new Ellipse2D.Double(-size/2, -size/2, size, size)
      pointRenderer.setShape(circle)
      pointRenderer.setColor(color)
      pointRenderer
    }
    def EMPTY_CIRCLE_POINT(size: Double = 5.0, color: Color = Color.BLACK): PointRenderer = {
      val innerSize = size - 1
      val pointRenderer = new DefaultPointRenderer2D
      val circle = new Area(new Ellipse2D.Double(-size/2, -size/2, size, size))
      circle.subtract(new Area(new Ellipse2D.Double(-innerSize/2, -innerSize/2, innerSize, innerSize)))
      pointRenderer.setShape(circle)
      pointRenderer.setColor(color)
      pointRenderer
    }
  }
}