package org.kai.stala.stat.est.impl

import org.apache.commons.math3.optim.{BaseOptimizer, InitialGuess, MaxEval, OptimizationData}
import org.apache.commons.math3.optim.nonlinear.scalar.GoalType
import org.apache.commons.math3.optim.nonlinear.scalar.noderiv.{NelderMeadSimplex, SimplexOptimizer}
import org.kai.stala.math.{ColVec, Mat, MatOption}
import org.kai.stala.stat.est.{PointValuePairHandler, _}
import org.kai.stala.stat.est.impl.GeneralizedLinearRegressionFormula.LinkageFunction

class GeneralizedLinearRegression(
  override val formula: GeneralizedLinearRegressionFormula,
  override val optimizer: BaseOptimizer[_],
  override val optimizeResultHandler: OptimizationResultHandler,
  override val optimizationData: Seq[OptimizationData]
) extends MaximumLikelihoodEstimator[MatSample,MatSample](
  formula,
  optimizer,
  optimizeResultHandler,
  optimizationData
)

object GeneralizedLinearRegression{
  def apply(
    formula: GeneralizedLinearRegressionFormula,
    maxEval: Int = 1000,
    initialGuess: Option[Seq[Double]] = None,
    optimizerAlgo: Option[OptimizationData] = None,
    optimizer: BaseOptimizer[_] = new SimplexOptimizer(1e-10, 1e-30),
    optimizeResultHandler: OptimizationResultHandler = PointValuePairHandler): GeneralizedLinearRegression = {
    require(initialGuess.forall(_.size == formula.numberOfParameters))
    val optimizerAlgoUsed = optimizerAlgo match {
      case Some(o) => Some(o)
      case None =>
        optimizer match {
          case _ : SimplexOptimizer =>
            Some(new NelderMeadSimplex(Array.fill[Double](formula.numberOfParameters)(0.2)))
          case _ => None
        }
    }
    val optimizationData: Seq[OptimizationData] = Seq(
      GoalType.MAXIMIZE,
      new InitialGuess(initialGuess.map(_.toArray).getOrElse(
        Array.fill[Double](formula.numberOfParameters)(0.0))),
      new MaxEval(maxEval)) ++ optimizerAlgoUsed
    new GeneralizedLinearRegression(formula, optimizer, optimizeResultHandler, optimizationData)
  }
}

object GeneralizedLinearRegressionFormula{
  trait LinkageFunction{
    def logLikelihood(y: Double, mu: Double): Double
    def inverseLinkage(y: Double): Double
  }
  object Distribution{
    object Normal extends LinkageFunction{
      override def logLikelihood(y: Double, mu: Double): Double = -(y-mu)*(y-mu)
      override def inverseLinkage(x: Double): Double = x
    }
    object Binomial extends LinkageFunction {
      override def logLikelihood(y: Double, mu: Double): Double = y * math.log(mu) + (1-y) * math.log(1-mu)
      override def inverseLinkage(x: Double): Double = 1/(1 + math.exp(-x))
    }
  }
}

class GeneralizedLinearRegressionFormula(
  beta: MatOption,
  linkageFunction: LinkageFunction
) extends Formula[MatSample, MatSample]{
  override def numberOfParameters: Int = beta.numberOfNone

  override def update(parameters: Seq[Double]): GeneralizedLinearRegressionCompleteFormula = {
    GeneralizedLinearRegressionCompleteFormula(beta.toMat(parameters))
  }

  override def logLikelihood(yObs: MatSample, xBeta: MatSample): Double = {
    (yObs.x.to1DVector, xBeta.x.to1DVector).zipped.map{
      case (y, n) => linkageFunction.logLikelihood(y, linkageFunction.inverseLinkage(n))
    }.sum
  }
}


case class GeneralizedLinearRegressionCompleteFormula(
  beta: Mat
) extends CompleteFormula[MatSample, MatSample] {
  override def fit(sample: MatSample): MatSample = {
    if (sample.x.width == beta.height)
      MatSample(sample.x * beta)
    else if (sample.x.width + 1 == beta.height) {
      MatSample(sample.x.cBind(ColVec.fill(sample.x.height, 1.0)) * beta)
    } else throw new IllegalArgumentException(s"Formula does not compile with sample, beta has ${beta.height} rows" +
      s"but sample has ${sample.x.width} columns")
  }
}