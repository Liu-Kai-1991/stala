package org.kai.stala.stat.filter

import org.junit.Test
import org.junit.Assert.assertEquals
import org.kai.stala.math.{ColVec, DenseMat, Mat}
import scala.math
import scala.util.Random

class KalmanFilterTest {

  //ref: https://commons.apache.org/proper/commons-math/userguide/filter.html
  @Test
  def increasingSpeedVehicle(): Unit = {
    // discrete time interval
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
    val B = Mat(Tuple1(math.pow(dt, 2d) / 2d ), Tuple1(dt))
    // H = [ 1 0 ]
    val H = Mat(1,0)
    // x = [ 0 ]
    //     [ 0 ]
    val x0 = ColVec(0,0)

    val tmp = Mat((math.pow(dt, 4d) / 4d, math.pow(dt, 3d) / 2d), (math.pow(dt, 3d) / 2d, math.pow(dt, 2d)))
    // Q = [ dt^4/4 dt^3/2 ]
    //     [ dt^3/2 dt^2   ]
    val Q = tmp * math.pow(accelNoise, 2)
    // P0 = [ 1 1 ]
    //      [ 1 1 ]
    val P0 = Mat((1,1), (1,1))
    // R = [ measurementNoise^2 ]
    val R = Mat(math.pow(measurementNoise, 2))

    // constant control input, increase velocity by 0.1 m/s per cycle
    val u = ColVec(0.1)

    val filter = KalmanFilter(A, B, H, Q, R, P0, x0)

    val rand = new Random(1)

    val tmpPNoise = ColVec(math.pow(dt, 2d) / 2d, dt )

    var x = x0

    val states = Range(0, 60).foldLeft(List(x0)){
      case (xs, _) =>
        // simulate the process
        val pNoise = tmpPNoise * (accelNoise * rand.nextGaussian)
        // x = A * x + B * u + pNoise
        (A * xs.head + B * u + pNoise).cols.head +: xs
    }.reverse

    val measures = states.map{
      x =>
        val mNoise = ColVec(measurementNoise * rand.nextGaussian)
        // z = H * x + m_noise
        (H * x + mNoise).cols.head
    }

    val estmations = measures.map{
      z =>
        filter.predict(u)
        filter.correct(z)
        filter.getStateEstimation
    }
    assertEquals(estmations.last(0), 0.7873890476141263, 1e-6)
    assertEquals(estmations.last(1), 0.34092009433194825, 1e-6)
    (states, measures, estmations).zipped.foreach{
      case (state, measure, est) =>
        println(s"< $state, | $measure")
        println(s"> $est")
    }
  }
}
