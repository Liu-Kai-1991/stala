package org.kai.stala.stat.filter

import java.awt.Color

import org.junit.Test
import org.junit.Assert.assertEquals
import org.kai.stala.math.{ ColVec, Mat }
import org.kai.stala.plot.{ MultiPlot, Plot, SeriesElement, SeriesPlot }

import scala.util.Random

//ref: https://commons.apache.org/proper/commons-math/userguide/filter.html
object IncreasingSpeedVehicle1 {
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

object IncreasingSpeedVehicle2 {

  // xk = Ax_{k-1} + Bu_{k-1} + w_{k-1}
  // zk = Hx_k + v_k.

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
  //     [ 0 1 ]
  val H = Mat((1, 0), (0, 1))
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
  val R = Mat((1, 0), (0, 1)) * math.pow(measurementNoise, 2)

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
      val mNoise = ColVec(measurementNoise * rand.nextGaussian, measurementNoise * rand.nextGaussian)
      // z = H * x + m_noise
      (H * x + mNoise).cols.head
  }
}

class KalmanFilterTest {

  @Test
  def increasingSpeedVehicle1(): Unit = {
    val filter = KalmanFilter(IncreasingSpeedVehicle1.A, IncreasingSpeedVehicle1.B, IncreasingSpeedVehicle1.H,
      IncreasingSpeedVehicle1.Q, IncreasingSpeedVehicle1.R, IncreasingSpeedVehicle1.P0, IncreasingSpeedVehicle1.x0)
    val estmations = IncreasingSpeedVehicle1.measures.map{
      z =>
        filter.predict(IncreasingSpeedVehicle1.u)
        filter.correct(z)
        filter.getStateEstimation
    }
    assertEquals(estmations.last(0), 0.7873890476141263, 1e-10)
    assertEquals(estmations.last(1), 0.34092009433194825, 1e-10)
//    val time = IncreasingSpeedVehicle1.states.indices.map(_ * IncreasingSpeedVehicle1.dt)
//    val realPositions = IncreasingSpeedVehicle1.states.map(_ (0))
//    val estmatedPositions = estmations.map(_ (0))
//    val realSpeed = IncreasingSpeedVehicle1.states.map(_ (1))
//    val estmatedSpeed = estmations.map(_ (1))
//    val observedPositions = IncreasingSpeedVehicle1.measures.map(_ (0))
//    val realPositionLine = SeriesElement(xs = time, ys = realPositions,
//      point = Plot.Point.CIRCLE_POINT(),
//      line = Plot.Line.SOLID_LINE(color = Color.blue),
//      name = "real position")
//    val estmatedPositionLine = SeriesElement(xs = time, ys = estmatedPositions,
//      point = Plot.Point.CIRCLE_POINT(),
//      line = Plot.Line.SOLID_LINE(color = Color.red),
//      name = "estimated position")
//    val observedPositionLine = SeriesElement(xs = time, ys = observedPositions,
//      point = Plot.Point.CIRCLE_POINT(),
//      line = Plot.Line.SOLID_LINE(color = Color.green),
//      name = "observed position")
//    val positionPlot = SeriesPlot(Seq(realPositionLine, observedPositionLine, estmatedPositionLine), legendVisible = true)
//    val estimatedSpeedLine = SeriesElement(xs = time, ys = estmatedSpeed,
//      point = Plot.Point.CIRCLE_POINT(),
//      line = Plot.Line.SOLID_LINE(color = Color.red),
//      name = "estimated speed")
//    val realSpeedLine = SeriesElement(xs = time, ys = realSpeed,
//      point = Plot.Point.CIRCLE_POINT(),
//      line = Plot.Line.SOLID_LINE(color = Color.blue),
//      name = "real speed")
//    val speedPlot = SeriesPlot(Seq(realSpeedLine, estimatedSpeedLine), legendVisible = true)
//    val combinedPlot = MultiPlot(Seq(positionPlot, speedPlot), syncNavigator = true)
//    combinedPlot.showInFrame
//    Thread.sleep(5000)
  }

  @Test
  def increasingSpeedVehicle2(): Unit = {
    val filter = KalmanFilter(IncreasingSpeedVehicle2.A, IncreasingSpeedVehicle2.B, IncreasingSpeedVehicle2.H,
      IncreasingSpeedVehicle2.Q, IncreasingSpeedVehicle2.R, IncreasingSpeedVehicle2.P0, IncreasingSpeedVehicle2.x0)
    val estmations = IncreasingSpeedVehicle2.measures.map{
      z =>
        filter.predict(IncreasingSpeedVehicle2.u)
        filter.correct(z)
        filter.getStateEstimation
    }
    assertEquals(0.7353271826685894, estmations.last(0), 1e-10)
    assertEquals(0.34170841718402645, estmations.last(1), 1e-10)
//    val time = IncreasingSpeedVehicle2.states.indices.map(_ * IncreasingSpeedVehicle2.dt)
//    val realPositions = IncreasingSpeedVehicle2.states.map(_ (0))
//    val estmatedPositions = estmations.map(_ (0))
//    val realSpeed = IncreasingSpeedVehicle2.states.map(_ (1))
//    val estmatedSpeed = estmations.map(_ (1))
//    val observedPositions = IncreasingSpeedVehicle2.measures.map(_ (0))
//    val observedSpeed = IncreasingSpeedVehicle2.measures.map(_ (1))
//    val realPositionLine = SeriesElement(xs = time, ys = realPositions,
//      point = Plot.Point.CIRCLE_POINT(),
//      line = Plot.Line.SOLID_LINE(color = Color.blue),
//      name = "real position")
//    val estmatedPositionLine = SeriesElement(xs = time, ys = estmatedPositions,
//      point = Plot.Point.CIRCLE_POINT(),
//      line = Plot.Line.SOLID_LINE(color = Color.red),
//      name = "estimated position")
//    val observedPositionLine = SeriesElement(xs = time, ys = observedPositions,
//      point = Plot.Point.CIRCLE_POINT(),
//      line = Plot.Line.SOLID_LINE(color = Color.green),
//      name = "observed position")
//    val positionPlot = SeriesPlot(Seq(realPositionLine, observedPositionLine, estmatedPositionLine), legendVisible = true)
//    val estimatedSpeedLine = SeriesElement(xs = time, ys = estmatedSpeed,
//      point = Plot.Point.CIRCLE_POINT(),
//      line = Plot.Line.SOLID_LINE(color = Color.red),
//      name = "estimated speed")
//    val realSpeedLine = SeriesElement(xs = time, ys = realSpeed,
//      point = Plot.Point.CIRCLE_POINT(),
//      line = Plot.Line.SOLID_LINE(color = Color.blue),
//      name = "real speed")
//    val observedSpeedLine = SeriesElement(xs = time, ys = observedSpeed,
//      point = Plot.Point.CIRCLE_POINT(),
//      line = Plot.Line.SOLID_LINE(color = Color.green),
//      name = "observed spped")
//    val speedPlot = SeriesPlot(Seq(realSpeedLine, estimatedSpeedLine, observedSpeedLine), legendVisible = true)
//    val combinedPlot = MultiPlot(Seq(positionPlot, speedPlot), syncNavigator = true)
//    combinedPlot.showInFrame
//    Thread.sleep(5000)
  }
}
