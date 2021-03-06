package org.kai.stala.stat.est

import org.apache.commons.math3.analysis.{ MultivariateFunction, UnivariateFunction }
import org.apache.commons.math3.optim.nonlinear.scalar.ObjectiveFunction
import org.apache.commons.math3.optim.univariate.UnivariateObjectiveFunction
import org.apache.commons.math3.optim.{ BaseOptimizer, OptimizationData }

class MaximumLikelihoodEstimator[X <: Sample[X], Y <: Sample[Y]](
  val formula: Formula[X, Y],
  val optimizer: BaseOptimizer[_],
  val optimizeResultHandler: OptimizationResultHandler,
  val optimizationData: Seq[OptimizationData]
) {
  def estimate(xSample: X, ySample: Y): OptimizationResult = {
    val objectiveFunction =
      if (formula.numberOfParameters > 1) {
        val multivariateFunction = new MultivariateFunction {
          override def value(point: Array[Double]): Double =
            formula.update(point).fitLogLikelihood(xSample, ySample)
        }
        new ObjectiveFunction(multivariateFunction)
      }
      else {
        val univariateFunction = new UnivariateFunction {
          override def value(x: Double): Double =
            formula.update(x).fitLogLikelihood(xSample, ySample)
        }
        new UnivariateObjectiveFunction(univariateFunction)
      }
    val optimizeResult = optimizer.optimize(objectiveFunction +: optimizationData: _*)
    optimizeResultHandler(optimizeResult)
  }

  def predict(xSample: X, parameters: Seq[Double]): Y = {
    formula.update(parameters).fit(xSample)
  }
}
