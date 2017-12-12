package org.kai.stala.stat.est

import org.apache.commons.math3.optim.PointValuePair

trait OptimizationResultHandler{
  def apply(x: Any): OptimizationResult
}

trait OptimizationResult

case class EstimatedParameter(parameters: Seq[Double]) extends OptimizationResult

object PointValuePairHandler extends OptimizationResultHandler {
  override def apply(x: Any): EstimatedParameter = EstimatedParameter(x.asInstanceOf[PointValuePair].getPoint)
}