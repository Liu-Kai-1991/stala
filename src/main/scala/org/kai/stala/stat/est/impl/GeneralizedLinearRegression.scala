package org.kai.stala.stat.est.impl

import org.apache.commons.math3.optim.{ BaseOptimizer, InitialGuess, MaxEval, OptimizationData }
import org.apache.commons.math3.optim.nonlinear.scalar.GoalType
import org.apache.commons.math3.optim.nonlinear.scalar.noderiv.{ NelderMeadSimplex, SimplexOptimizer }
import org.apache.commons.math3.optim.univariate.{ BrentOptimizer, SearchInterval }
import org.kai.stala.math.{ ColVec, Mat, MatOption }
import org.kai.stala.stat.est.{ PointValuePairHandler, _ }
import org.kai.stala.stat.est.impl.GeneralizedLinearRegressionFormula.LinkageFunction

class GeneralizedLinearRegression(
  override val formula: GeneralizedLinearRegressionFormula,
  override val optimizer: BaseOptimizer[_],
  override val optimizeResultHandler: OptimizationResultHandler,
  override val optimizationData: Seq[OptimizationData]
) extends MaximumLikelihoodEstimator[MatSample, MatSample](
  formula,
  optimizer,
  optimizeResultHandler,
  optimizationData
)

object GeneralizedLinearRegression {
  val defaultUpperBound: Double = 1e20
  val defaultLowerBound: Double = -1e20

  def buildFromSample(
    y: MatSample,
    x: MatSample,
    linkageFunction: LinkageFunction,
    includeIntercept: Boolean = true,
    maxEval: Int = 10000,
    initialGuessOption: Option[Seq[Double]] = None,
    optimizerAlgoOption: Option[OptimizationData] = None,
    optimizerOption: Option[BaseOptimizer[_]] = None,
    optimizeResultHandler: OptimizationResultHandler = PointValuePairHandler): GeneralizedLinearRegression = {
    val beta = MatOption.empty(x.x.width + (if (includeIntercept) 1 else 0), y.x.width)
    val formula = new GeneralizedLinearRegressionFormula(beta, linkageFunction)
    apply(formula, maxEval, initialGuessOption, optimizerAlgoOption, optimizerOption, optimizeResultHandler)
  }

  def apply(
    formula: GeneralizedLinearRegressionFormula,
    maxEval: Int = 10000,
    initialGuessOption: Option[Seq[Double]] = None,
    optimizerAlgoOption: Option[OptimizationData] = None,
    optimizerOption: Option[BaseOptimizer[_]] = None,
    optimizeResultHandler: OptimizationResultHandler = PointValuePairHandler): GeneralizedLinearRegression = {
    val initialGuess = initialGuessOption.getOrElse(Seq.fill[Double](formula.numberOfParameters)(0.0))
    require(initialGuess.size == formula.numberOfParameters)
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
      new SearchInterval(defaultLowerBound, defaultUpperBound)) ++ optimizerAlgoUsed
    new GeneralizedLinearRegression(formula, optimizer, optimizeResultHandler, optimizationData)
  }
}

object GeneralizedLinearRegressionFormula {
  trait LinkageFunction {
    def logLikelihood(y: Double, mu: Double): Double
    def inverseLinkage(y: Double): Double
  }
  object Distribution {
    object Normal extends LinkageFunction {
      override def logLikelihood(y: Double, mu: Double): Double = -(y - mu) * (y - mu)
      override def inverseLinkage(x: Double): Double = x
    }
    object Binomial extends LinkageFunction {
      override def logLikelihood(y: Double, mu: Double): Double = y * math.log(mu) + (1 - y) * math.log(1 - mu)
      override def inverseLinkage(x: Double): Double = 1 / (1 + math.exp(-x))
    }
    object Poisson extends LinkageFunction {
      override def logLikelihood(y: Double, mu: Double): Double = y * math.log(mu) - mu
      override def inverseLinkage(x: Double): Double = math.exp(x)
    }
  }
}

class GeneralizedLinearRegressionFormula(
  betaOption: MatOption,
  linkageFunction: LinkageFunction
) extends Formula[MatSample, MatSample] {
  override val numberOfParameters: Int = betaOption.numberOfNone
  override def update(parameters: Seq[Double]): GeneralizedLinearRegressionCompleteFormula = {
    GeneralizedLinearRegressionCompleteFormula(betaOption.toMat(parameters), betaOption, linkageFunction)
  }
}

case class GeneralizedLinearRegressionCompleteFormula(
  beta: Mat,
  betaOption: MatOption,
  linkageFunction: LinkageFunction
) extends GeneralizedLinearRegressionFormula(betaOption, linkageFunction)
  with CompleteFormula[MatSample, MatSample] {
  override def fit(sample: MatSample): MatSample = {
    if (sample.x.width == beta.height)
      MatSample(sample.x * beta)
    else if (sample.x.width + 1 == beta.height) {
      MatSample(sample.x.cBind(ColVec.fill(sample.x.height, 1.0)) * beta)
    } else throw new IllegalArgumentException(s"Formula does not compile with sample, beta has ${ beta.height } rows" +
      s"but sample has ${ sample.x.width } columns")
  }
  override def logLikelihoodCalc(yObs: MatSample, xBeta: MatSample): Double = {
    (yObs.x.to1DVector, xBeta.x.to1DVector).zipped.map{
      case (y, n) => linkageFunction.logLikelihood(y, linkageFunction.inverseLinkage(n))
    }.sum
  }
}