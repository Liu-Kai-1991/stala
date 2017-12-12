package org.kai.stala.stat.est

trait Formula[X <: Sample[X], Y <: Sample[Y]] {
  def logLikelihood(y: Y, estimated: Y): Double
  def update(parameters: Seq[Double]): CompleteFormula[X, Y]
  def numberOfParameters: Int
}

trait CompleteFormula[X <: Sample[X], Y <: Sample[Y]]{
  def fit(sample: X): Y
}