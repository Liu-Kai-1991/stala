package org.kai.stala.stat.est

import org.kai.stala.math.Mat

trait Sample[SampleType <: Sample[SampleType]]

case class MatSample(x: Mat) extends Sample[MatSample]

case class SeqSample[T](s: Seq[T]) extends Sample[SeqSample[T]]

trait DummySample extends Sample[DummySample]
case object DummySample extends DummySample