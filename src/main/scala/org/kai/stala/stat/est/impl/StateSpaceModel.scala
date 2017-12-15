package org.kai.stala.stat.est.impl

import org.apache.commons.math3.optim.nonlinear.scalar.GoalType
import org.apache.commons.math3.optim.nonlinear.scalar.noderiv.{ NelderMeadSimplex, SimplexOptimizer }
import org.apache.commons.math3.optim.univariate.{ BrentOptimizer, SearchInterval }
import org.apache.commons.math3.optim.{ BaseOptimizer, InitialGuess, MaxEval, OptimizationData }
import org.kai.stala.math._
import org.kai.stala.stat.est._
import org.kai.stala.stat.est.impl.GeneralizedLinearRegression.{ defaultLowerBound, defaultUpperBound }
import org.kai.stala.stat.filter.KalmanFilter

class StateSpaceModel(
  override val formula: StateSpaceModelFormula,
  override val optimizer: BaseOptimizer[_],
  override val optimizeResultHandler: OptimizationResultHandler,
  override val optimizationData: Seq[OptimizationData]
) extends MaximumLikelihoodEstimator[SeqSample[(ColVec, ColVec)], SeqSample[(ColVec, Mat)]](
  formula,
  optimizer,
  optimizeResultHandler,
  optimizationData
) {
  def estimate(xSample: SeqSample[(ColVec, ColVec)]): OptimizationResult =
    super.estimate(xSample, SeqSample(xSample.s.map{
      case (u, z) => (z, DummyMat)
    }))
}

object StateSpaceModel {
  def apply(
    A: MatOption,
    B: MatOption,
    H: MatOption,
    Q: MatOption,
    R: MatOption,
    P0: MatOption,
    x0: ColVecOption,
    maxEval: Int = 10000,
    initialGuessOption: Option[Seq[Double]] = None,
    optimizerOption: Option[BaseOptimizer[_]] = None,
    optimizeResultHandler: OptimizationResultHandler = PointValuePairHandler,
    optimizerAlgoOption: Option[OptimizationData] = None,
  ): StateSpaceModel = {
    val formula = new StateSpaceModelFormula(A, B, H, Q, R, P0, x0)
    val initialGuess = initialGuessOption.getOrElse(Seq.fill[Double](formula.numberOfParameters)(0.0))
    val optimizer = optimizerOption.getOrElse(
      if (formula.numberOfParameters > 1) new SimplexOptimizer(1e-10, 1e-30) else new BrentOptimizer(1e-10, 1e-30)
    )
    val optimizerAlgoUsed = optimizerAlgoOption match {
      case Some(o) => Some(o)
      case None =>
        optimizer match {
          case _: SimplexOptimizer =>
            Some(new NelderMeadSimplex(Array.fill[Double](formula.numberOfParameters)(0.2)))
          case _: BrentOptimizer =>
            None
          case _ =>
            None
        }
    }
    val optimizationData: Seq[OptimizationData] = Seq(
      GoalType.MAXIMIZE,
      new InitialGuess(initialGuess.toArray),
      new MaxEval(maxEval),
      new SearchInterval(-1, 1)) ++ optimizerAlgoUsed
    new StateSpaceModel(formula, optimizer, optimizeResultHandler, optimizationData)
  }
}

class StateSpaceModelFormula(
  A: MatOption,
  B: MatOption,
  H: MatOption,
  Q: MatOption,
  R: MatOption,
  P0: MatOption,
  x0: ColVecOption,
) extends Formula[SeqSample[(ColVec, ColVec)], SeqSample[(ColVec, Mat)]] {
  override val numberOfParameters: Int = Seq(A, B, H, Q, R, P0, x0).map(_.numberOfNone).sum

  override def update(parameters: Seq[Double]): StateSpaceModelCompleteFormula = {
    val iter = parameters.toIterator
    StateSpaceModelCompleteFormula(
      A.toMat(iter),
      B.toMat(iter),
      H.toMat(iter),
      Q.toMat(iter),
      R.toMat(iter),
      P0.toMat(iter),
      x0.toVec(iter),
      A: MatOption,
      B: MatOption,
      H: MatOption,
      Q: MatOption,
      R: MatOption,
      P0: MatOption,
      x0: ColVecOption)
  }
}

case class StateSpaceModelCompleteFormula(
  A: Mat,
  B: Mat,
  H: Mat,
  Q: Mat,
  R: Mat,
  P0: Mat,
  x0: ColVec,
  AOption: MatOption,
  BOption: MatOption,
  HOption: MatOption,
  QOption: MatOption,
  ROption: MatOption,
  P0Option: MatOption,
  x0Option: ColVecOption,
) extends StateSpaceModelFormula(
  AOption,
  BOption,
  HOption,
  QOption,
  ROption,
  P0Option,
  x0Option,
) with CompleteFormula[SeqSample[(ColVec, ColVec)], SeqSample[(ColVec, Mat)]] {
  override def fit(sample: SeqSample[(ColVec, ColVec)]): SeqSample[(ColVec, Mat)] = {
    val filter = KalmanFilter(A, B, H, Q, R, P0, x0)
    println(filter.A)
    val res = SeqSample(filter.process(sample.s))
    res
  }

  /** KalmanFilter
   *
   * xk = Ax_{k-1} + Bu_{k-1} + w_{k-1}
   * zk = Hx_k + v_k.
   *
   * @param y SeqSample[(z: ColVec, _: Mat)]
   * @param estimated SeqSample[(x: ColVec, P = Cov(x): Mat)]
   * @return a new Person instance with the age determined by the
   *         birthdate and current date.
   */
  override def logLikelihoodCalc(y: SeqSample[(ColVec, Mat)], estimated: SeqSample[(ColVec, Mat)]): Double = {
    val res =
    (y.s, estimated.s).zipped.map{
      case ((obs, _), (est, cov)) =>
        val diff = obs - H * est
        val covObs = H * cov * H.transpose + R
        -(math.log(covObs.determinant) + (diff.transpose * covObs * diff).apply(0, 0))
    }.sum
    println(res)
    res
  }

  override def logLikelihoodReal(y: SeqSample[(ColVec, Mat)], estimated: SeqSample[(ColVec, Mat)]): Double = {
    0.5 * (logLikelihoodCalc(y, estimated) + H.height * math.log(math.Pi))
  }
}