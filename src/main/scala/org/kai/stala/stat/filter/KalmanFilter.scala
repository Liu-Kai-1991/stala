package org.kai.stala.stat.filter

import org.kai.stala.math.{ ColVec, DenseMat, Mat, RowVec }
import org.apache.commons.math3.filter.DefaultMeasurementModel
import org.apache.commons.math3.filter.DefaultProcessModel
import org.apache.commons.math3.filter.{ KalmanFilter => JKalmanFilter }
import org.apache.commons.math3.linear.CholeskyDecomposition
import org.apache.commons.math3.linear.MatrixUtils
import org.apache.commons.math3.linear.RealMatrix
import org.apache.commons.math3.linear.RealVector

/** KalmanFilter
 *
 * This is scala Kalman filter clone for org.apache.commons.math3.filter.KalmanFilter
 * xk = Ax_{k-1} + Bu_{k-1} + w_{k-1}
 * zk = Hx_k + v_k.
 *
 * A - state transition matrix
 * B - control input matrix
 * H - measurement matrix
 * Q - process noise covariance matrix
 * R - measurement noise covariance matrix
 * P0 - initial error covariance matrix
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

  private var stateEstimation = x0.realVector
  private var errorCovariance = P0.realMatrix
  private val transitionMatrixT = A.realMatrix.transpose
  private val measurementMatrixT = H.realMatrix.transpose

  def process(input: Seq[(ColVec, ColVec)]): Seq[(ColVec, Mat)] =
    input.map{
      case (u, z) =>
        predict(u)
        correct(z)
        (getStateEstimation, getErrorCovariance)
    }

  final def process(us: Seq[ColVec], zs: Seq[ColVec]): Seq[(ColVec, Mat)] = {
    require(us.lengthCompare(zs.size) == 0)
    process((us, zs).zipped.toSeq)
  }

  def predict(u: ColVec): Unit = {
    require(u.height == B.width)
    // project the state estimation ahead (a priori state)
    // xHat(k)- = A * xHat(k-1) + B * u(k-1)
    stateEstimation = A.realMatrix.operate(stateEstimation).add(B.realMatrix.operate(u.realVector))
    // project the error covariance ahead
    // P(k)- = A * P(k-1) * A' + Q
    errorCovariance = A.realMatrix.multiply(errorCovariance).multiply(transitionMatrixT).add(Q.realMatrix)
  }

  def predict(): Unit = {
    // project the state estimation ahead (a priori state)
    // xHat(k)- = A * xHat(k-1)
    stateEstimation = A.realMatrix.operate(stateEstimation)
    // project the error covariance ahead
    // P(k)- = A * P(k-1) * A' + Q
    errorCovariance = A.realMatrix.multiply(errorCovariance).multiply(transitionMatrixT).add(Q.realMatrix)
  }

  def correct(z: ColVec): Unit = {
    require(z.height == H.height)
    val s = H.realMatrix.multiply(errorCovariance).multiply(measurementMatrixT).add(R.realMatrix)
    // Inn = z(k) - H * xHat(k)-
    val innovation = z.realVector.subtract(H.realMatrix.operate(stateEstimation))
    // calculate gain matrix
    // K(k) = P(k)- * H' * (H * P(k)- * H' + R)^-1
    // K(k) = P(k)- * H' * S^-1
    // instead of calculating the inverse of S we can rearrange the formula,
    // and then solve the linear equation A x X = B with A = S', X = K' and B = (H * P)'
    // K(k) * S = P(k)- * H'
    // S' * K(k)' = H * P(k)-'
    val kalmanGain = new CholeskyDecomposition(s, CholeskyDecomposition.DEFAULT_ABSOLUTE_POSITIVITY_THRESHOLD,
      CholeskyDecomposition.DEFAULT_ABSOLUTE_POSITIVITY_THRESHOLD).getSolver
      .solve(H.realMatrix.multiply(errorCovariance.transpose)).transpose

    // update estimate with measurement z(k)
    // xHat(k) = xHat(k)- + K * Inn
    stateEstimation = stateEstimation.add(kalmanGain.operate(innovation))

    // update covariance of prediction error
    // P(k) = (I - K * H) * P(k)-
    val identity = MatrixUtils.createRealIdentityMatrix(kalmanGain.getRowDimension)
    errorCovariance = identity.subtract(kalmanGain.multiply(H.realMatrix)).multiply(errorCovariance)
  }

  def getStateEstimation: ColVec =
    ColVec(stateEstimation)

  def getErrorCovariance: Mat =
    DenseMat(errorCovariance)
}


