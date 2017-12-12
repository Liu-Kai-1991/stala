package org.kai.stala.stat.est

import org.kai.stala.math.Mat

trait Sample[SampleType <: Sample[SampleType]]

case class MatSample(x: Mat) extends Sample[MatSample]