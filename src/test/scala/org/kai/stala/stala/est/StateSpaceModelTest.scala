package org.kai.stala.stala.est

import org.junit.Test
import org.kai.stala.math.{ ColVecOption, MatOption }
import org.kai.stala.stat.est.SeqSample
import org.kai.stala.stat.est.impl.StateSpaceModel
import org.kai.stala.stat.filter.{ IncreasingSpeedVehicle, KalmanFilter }

class StateSpaceModelTest {

  @Test
  def increasingSpeedVehicle(): Unit = {

    val A = MatOption((None, IncreasingSpeedVehicle.dt), (0, 1))

    val ssm = StateSpaceModel(A, MatOption(IncreasingSpeedVehicle.B), MatOption(IncreasingSpeedVehicle.H),
      MatOption(IncreasingSpeedVehicle.Q), MatOption(IncreasingSpeedVehicle.R), MatOption(IncreasingSpeedVehicle.P0),
      ColVecOption(IncreasingSpeedVehicle.x0))

    val seqSample = SeqSample(IncreasingSpeedVehicle.measures.map(measure => (IncreasingSpeedVehicle.u, measure)))

    val res = ssm.estimate(seqSample)

    println(res)

    val estimated = ssm.predict(seqSample, res.parameters)

    println(estimated.s.map(_._1))
  }
}
