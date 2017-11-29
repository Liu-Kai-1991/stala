package org.kai.stala.stat.filter

import org.kai.stala.math.{ColVec, DenseMat, Mat, RowVec}
import org.apache.commons.math3.filter.DefaultMeasurementModel
import org.apache.commons.math3.filter.DefaultProcessModel
import org.apache.commons.math3.filter.{KalmanFilter => JKalmanFilter}

/** KalmanFilter
  *
  *  This is scala Kalman filter which wraps org.apache.commons.math3.filter.KalmanFilter
  *   xk = Ax_{k-1} + Bu_{k-1} + w_{k-1}
  *   zk = Hx_k + v_k.
  *
  *   A - state transition matrix
  *   B - control input matrix
  *   H - measurement matrix
  *   Q - process noise covariance matrix
  *   R - measurement noise covariance matrix
  *   P0 - initial error covariance matrix
  */

case class KalmanFilter(
  A: Mat,
  B: Mat,
  H: Mat,
  Q: Mat,
  R: Mat,
  P0: Mat,
  x0: ColVec
) {
  require(A.isSquare, "matrix A should be square matrix")
  val xLength: Int = A.height
  val zLength: Int = H.height
  require(B.height == xLength, "matrix B should has height as matrix A")
  require(Q.isSquare && Q.height == xLength, "matrix Q should has same shape as matrix A")
  require(H.width == xLength, "matrix H should has width as matrix A")
  require(R.isSquare && R.height == zLength, "matrix R should has same shape as matrix H")
  require(P0.isSquare && P0.height == xLength, "matrix P0 should has same shape as matrix A")

  val pm = new DefaultProcessModel(A.realMatrix, B.realMatrix, Q.realMatrix, x0.realVector, P0.realMatrix)
  val mm = new DefaultMeasurementModel(H.realMatrix, R.realMatrix)
  val filter = new JKalmanFilter(pm, mm)

  def predict(u: ColVec): Unit =
    filter.predict(u.realVector)
  def correct(z: ColVec): Unit =
    filter.correct(z.realVector)
  def getStateEstimation: ColVec =
    ColVec(filter.getStateEstimationVector)
  def getErrorCovariance: Mat =
    DenseMat(filter.getErrorCovarianceMatrix)
}


