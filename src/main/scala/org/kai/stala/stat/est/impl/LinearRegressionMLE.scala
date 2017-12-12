package org.kai.stala.stat.est.impl

import org.apache.commons.math3.distribution.NormalDistribution
import org.apache.commons.math3.optim.nonlinear.scalar.GoalType
import org.apache.commons.math3.optim.{InitialGuess, MaxEval, OptimizationData, PointValuePair}
import org.apache.commons.math3.optim.nonlinear.scalar.noderiv.{NelderMeadSimplex, SimplexOptimizer}
import org.kai.stala.math.{ColVec, DenseMat, Mat}
import org.kai.stala.stat.est._

class LinearRegressionMLE(
  override val formula: LinearRegressionMLEFormula
) extends MaximumLikelihoodEstimator[MatSample, MatSample](
  formula,
  new SimplexOptimizer(1e-10, 1e-30),
  PointValuePairHandler,
  Seq(GoalType.MAXIMIZE,
    new NelderMeadSimplex(Array[Double](0.2, 0.2)),
    new InitialGuess(Array(0.0, 0.0)),
    new MaxEval(10000))) {
  override val optimizer = new SimplexOptimizer(1e-10, 1e-30)
}

class LinearRegressionMLEFormula(
  val numberOfParameters: Int
) extends Formula[MatSample, MatSample]{
  override def update(parameters: Seq[Double]): LinearRegressionMLECompleteFormula = {
    LinearRegressionMLECompleteFormula(numberOfParameters, ColVec(parameters :_*))
  }

  override def logLikelihood(y: MatSample, yEst: MatSample): Double = {
    val flatten = (y.x - yEst.x).to1DVector
    val distribution = new NormalDistribution(mean(flatten), std(flatten))
    flatten.map(distribution.density).map(math.log).sum
  }
}

case class LinearRegressionMLECompleteFormula(
  override val numberOfParameters: Int,
  beta: Mat
) extends LinearRegressionMLEFormula(numberOfParameters) with CompleteFormula[MatSample, MatSample] {
  override def fit(sample: MatSample): MatSample = {
    if (sample.x.width == beta.height)
      MatSample(sample.x * beta)
    else {
      ???
    }
  }
}