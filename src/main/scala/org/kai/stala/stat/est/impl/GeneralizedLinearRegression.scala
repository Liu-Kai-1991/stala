package org.kai.stala.stat.est.impl

import org.apache.commons.math3.distribution.NormalDistribution
import org.apache.commons.math3.optim.{BaseOptimizer, OptimizationData}
import org.apache.commons.math3.optim.nonlinear.scalar.GoalType
import org.apache.commons.math3.optim.nonlinear.scalar.noderiv.{NelderMeadSimplex, SimplexOptimizer}
import org.kai.stala.math.{ColVec, Mat, MatOption}
import org.kai.stala.stat.est._
import org.kai.stala.stat.est.impl.GeneralizedLinearRegressionFormula.DistributionAndLinkageFunction

class GeneralizedLinearRegression(
  override val formula: Formula[MatSample, MatSample],
  override val optimizer: BaseOptimizer[_],
  override val optimizeResultHandler: OptimizationResultHandler,
  override val optimizationData: Seq[OptimizationData]
) extends MaximumLikelyhoodEstimator[MatSample,MatSample](
  formula,
  optimizer,
  optimizeResultHandler,
  optimizationData
)

object GeneralizedLinearRegression{


}

object GeneralizedLinearRegressionFormula{
  trait DistributionAndLinkageFunction{
    def density(x: Double): Double
    def linkage(x: Double): Double
  }
  object Distribution{
    object Normal extends DistributionAndLinkageFunction{
      val dist = new NormalDistribution
      override def density(x: Double): Double = (new NormalDistribution).density(x)
      override def linkage(x: Double): Double = x
    }
  }
}

class GeneralizedLinearRegressionFormula(
  matOption: MatOption,
  distributionAndLinkageFunction: DistributionAndLinkageFunction
) extends Formula[MatSample, MatSample]{
  override def numberOfParameters: Int = matOption.numberOfNone

  override def update(parameters: Seq[Double]): LinearRegressionMLECompleteFormula = {
    LinearRegressionMLECompleteFormula(numberOfParameters, ColVec(parameters :_*))
  }

  override def likelihood(residual: MatSample): Double = {
    val flatten = residual.x.to1DVector
    val distribution = new NormalDistribution(mean(flatten), std(flatten))
    flatten.map(distribution.density).map(math.log).sum
  }
}


case class GeneralizedLinearRegressionCompleteFormula(
  beta: Mat
) extends CompleteFormula[MatSample, MatSample] {
  override def fit(sample: MatSample): MatSample = {
    if (sample.x.width == beta.height)
      MatSample(sample.x * beta)
    else {
      ???
    }
  }
}