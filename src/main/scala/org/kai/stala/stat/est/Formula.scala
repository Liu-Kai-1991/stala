package org.kai.stala.stat.est

trait Formula[X <: Sample[X], Y <: Sample[Y]] {
  def update(parameters: Seq[Double]): CompleteFormula[X, Y]
  def update(parameter: Double): CompleteFormula[X, Y] = update(Seq(parameter))
  def numberOfParameters: Int
}

trait CompleteFormula[X <: Sample[X], Y <: Sample[Y]] extends Formula[X, Y] {
  def logLikelihoodCalc(y: Y, estimated: Y): Double
  def logLikelihoodReal(y: Y, estimated: Y): Double = ???
  def fit(sample: X): Y
  final def fitLogLikelihood(sample: X, y: Y): Double = logLikelihoodCalc(y, fit(sample))
}
