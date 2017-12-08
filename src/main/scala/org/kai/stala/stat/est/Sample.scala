package org.kai.stala.stat.est

import org.kai.stala.math.Mat

trait Sample[SampleType <: Sample[SampleType]] {
  def residual(compare: SampleType): SampleType
}

case class MatSample(x: Mat) extends Sample[MatSample] {
  override def residual(compare: MatSample): MatSample = MatSample(compare .x- x)
}