package org.kai.stala.stat.filter

import java.awt.Color

import org.junit.Test
import org.junit.Assert.assertEquals
import org.kai.stala.math.{ ColVec, Mat }
import org.kai.stala.plot.{ MultiPlot, Plot, SeriesElement, SeriesPlot }

import scala.util.Random

//ref: https://commons.apache.org/proper/commons-math/userguide/filter.html
object IncreasingSpeedVehicle {
  val dt = 0.1d
  // position measurement noise (meter)
  val measurementNoise = 0.10d
  // acceleration noise (meter/sec^2)
  val accelNoise = 0.2d
  // A = [ 1 dt ]
  //     [ 0  1 ]
  val A = Mat((1.0, dt), (0.0, 1.0))
  // B = [ dt^2/2 ]
  //     [ dt     ]
  val B = Mat(Tuple1(math.pow(dt, 2d) / 2d), Tuple1(dt))
  // H = [ 1 0 ]
  val H = Mat(1, 0)
  // x = [ 0 ]
  //     [ 0 ]
  val x0 = ColVec(0, 0)

  val tmp = Mat((math.pow(dt, 4d) / 4d, math.pow(dt, 3d) / 2d), (math.pow(dt, 3d) / 2d, math.pow(dt, 2d)))
  // Q = [ dt^4/4 dt^3/2 ]
  //     [ dt^3/2 dt^2   ]
  val Q: Mat = tmp * math.pow(accelNoise, 2)
  // P0 = [ 1 1 ]
  //      [ 1 1 ]
  val P0 = Mat((1, 1), (1, 1))
  // R = [ measurementNoise^2 ]
  val R = Mat(math.pow(measurementNoise, 2))

  // constant control input, increase velocity by 0.1 m/s per cycle
  val u = ColVec(0.1)

  val rand = new Random(1)

  val tmpPNoise = ColVec(math.pow(dt, 2d) / 2d, dt)

  var x: ColVec = x0

  val states: List[ColVec] = Range(0, 60).foldLeft(List(x0)){
    case (xs, _) =>
      // simulate the process
      val pNoise = tmpPNoise * (accelNoise * rand.nextGaussian)
      // x = A * x + B * u + pNoise
      (A * xs.head + B * u + pNoise).cols.head +: xs
  }.reverse

  val measures: List[ColVec] = states.map{
    x =>
      val mNoise = ColVec(measurementNoise * rand.nextGaussian)
      // z = H * x + m_noise
      (H * x + mNoise).cols.head
  }
}

class KalmanFilterTest {

  @Test
  def increasingSpeedVehicle(): Unit = {
    val filter = KalmanFilter(IncreasingSpeedVehicle.A, IncreasingSpeedVehicle.B, IncreasingSpeedVehicle.H,
      IncreasingSpeedVehicle.Q, IncreasingSpeedVehicle.R, IncreasingSpeedVehicle.P0, IncreasingSpeedVehicle.x0)
    val estmations = IncreasingSpeedVehicle.measures.map{
      z =>
        filter.predict(IncreasingSpeedVehicle.u)
        filter.correct(z)
        filter.getStateEstimation
    }
    assertEquals(estmations.last(0), 0.7873890476141263, 1e-6)
    assertEquals(estmations.last(1), 0.34092009433194825, 1e-6)
    val time = IncreasingSpeedVehicle.states.indices.map(_ * IncreasingSpeedVehicle.dt)
    val realPositions = IncreasingSpeedVehicle.states.map(_ (0))
    val estmatedPositions = estmations.map(_ (0))
    val realSpeed = IncreasingSpeedVehicle.states.map(_ (1))
    val estmatedSpeed = estmations.map(_ (1))
    val observedPositions = IncreasingSpeedVehicle.measures.map(_ (0))
    val realPositionLine = SeriesElement(xs = time, ys = realPositions,
      point = Plot.Point.CIRCLE_POINT(),
      line = Plot.Line.SOLID_LINE(color = Color.blue),
      name = "real position")
    val estmatedPositionLine = SeriesElement(xs = time, ys = estmatedPositions,
      point = Plot.Point.CIRCLE_POINT(),
      line = Plot.Line.SOLID_LINE(color = Color.red),
      name = "estimated position")
    val observedPositionLine = SeriesElement(xs = time, ys = observedPositions,
      point = Plot.Point.CIRCLE_POINT(),
      line = Plot.Line.SOLID_LINE(color = Color.green),
      name = "observed position")
    val positionPlot = SeriesPlot(Seq(realPositionLine, observedPositionLine, estmatedPositionLine), legendVisible = true)
    val estimatedSpeedLine = SeriesElement(xs = time, ys = estmatedSpeed,
      point = Plot.Point.CIRCLE_POINT(),
      line = Plot.Line.SOLID_LINE(color = Color.red),
      name = "estimated speed")
    val realSpeedLine = SeriesElement(xs = time, ys = realSpeed,
      point = Plot.Point.CIRCLE_POINT(),
      line = Plot.Line.SOLID_LINE(color = Color.blue),
      name = "real speed")
    val speedPlot = SeriesPlot(Seq(realSpeedLine, estimatedSpeedLine), legendVisible = true)
    val combinedPlot = MultiPlot(Seq(positionPlot, speedPlot), syncNavigator = true)
    combinedPlot.showInFrame
    Thread.sleep(50000)
  }
}
