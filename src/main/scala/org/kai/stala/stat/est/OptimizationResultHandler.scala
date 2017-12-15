package org.kai.stala.stat.est

import org.apache.commons.math3.optim.PointValuePair
import org.apache.commons.math3.optim.univariate.UnivariatePointValuePair

trait OptimizationResultHandler {
  def apply(x: Any): OptimizationResult
}

trait OptimizationResult {
  def parameters: Seq[Double]
  def value: Double
}

case class EstimatedParameter(
  parameters: Seq[Double],
  value: Double) extends OptimizationResult

object PointValuePairHandler extends OptimizationResultHandler {
  override def apply(x: Any): EstimatedParameter = x match {
    case v: UnivariatePointValuePair => EstimatedParameter(Seq(v.getPoint), v.getValue)
    case v: PointValuePair => EstimatedParameter(v.getPoint, v.getValue)
  }
}