package org.kai.stala.stala.est

import java.awt.Color

import org.junit.Test
import org.kai.stala.math.{ ColVecOption, MatOption }
import org.junit.Assert.assertEquals
import org.kai.stala.stat.est.SeqSample
import org.kai.stala.stat.est.impl.StateSpaceModel
import org.kai.stala.stat.filter.IncreasingSpeedVehicle2
import org.kai.stala.stat.filter.{ IncreasingSpeedVehicle1, KalmanFilter }

import scala.util.Random

class StateSpaceModelTest {

  @Test
  def increasingSpeedVehicle1(): Unit = {
    val A = MatOption((None, IncreasingSpeedVehicle1.dt), (0, 1))
    val ssm = StateSpaceModel(A, MatOption(IncreasingSpeedVehicle1.B), MatOption(IncreasingSpeedVehicle1.H),
      MatOption(IncreasingSpeedVehicle1.Q), MatOption(IncreasingSpeedVehicle1.R), MatOption(IncreasingSpeedVehicle1.P0),
      ColVecOption(IncreasingSpeedVehicle1.x0), searchIntervalOption = Some((-2, 2)))
    val seqSample = SeqSample(IncreasingSpeedVehicle1.measures.map(measure => (IncreasingSpeedVehicle1.u, measure)))
    val res = ssm.estimate(seqSample)
    assertEquals(0.9796439364205546, res.parameters.head, 1e-10)

  }
}
