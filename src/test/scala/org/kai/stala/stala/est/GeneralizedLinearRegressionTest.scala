package org.kai.stala.stala.est

import org.apache.commons.math3.distribution.{BinomialDistribution, PoissonDistribution}
import org.junit.Test
import org.kai.stala.math.{ColVec, ColVecOption, Mat}
import org.kai.stala.stat.est.MatSample
import org.kai.stala.stat.est.impl.{GeneralizedLinearRegression, GeneralizedLinearRegressionFormula, LinearRegressionMLE, LinearRegressionMLEFormula}
import org.junit.Assert._

import scala.util.Random

class GeneralizedLinearRegressionTest {
  Random.setSeed(0)
  // y = 3 * x1 + 7 * x2 + 1
  val obsSize = 500
  val x: Mat = Mat.from1DVector(obsSize, 2, Range(0, obsSize*2).map(_ => Random.nextGaussian()))
  val beta: ColVec = ColVec(3,7)
  val y1: Mat = x * beta + ColVec(Range(0, obsSize).map(_ => Random.nextGaussian() * 2 + 1))
  val logistic: Mat = ColVec(y1.to1DVector.map(GeneralizedLinearRegressionFormula.Distribution.Binomial.inverseLinkage))
  val y2: Mat = ColVec(logistic.to1DVector.map{p => if (p > Random.nextDouble) 1 else 0})
  //y3 = 1 * x1 + 2 * x2 + 20
  val beta3 = ColVec(1,2)
  val y3: Mat = ColVec((x * beta3 + ColVec(Range(0, obsSize).map(_ => Random.nextGaussian() + 8))).to1DVector.map(mu => new PoissonDistribution(mu).sample()))


  @Test
  def normal1(): Unit ={
    val formula = new GeneralizedLinearRegressionFormula(
      ColVecOption(None, None, None),
      GeneralizedLinearRegressionFormula.Distribution.Normal)
    val estimator = GeneralizedLinearRegression(formula)
    val estBeta = estimator.estimate(MatSample(x), MatSample(y1))
    assertArrayEquals(Array(2.93425072514275, 7.015427560018422, 1.1781502495819423),
      estBeta.parameters.toArray, 1e-10)
  }

  @Test
  def normal1a(): Unit ={
    val estimator = GeneralizedLinearRegression.buildFromSample(MatSample(y1), MatSample(x),
      GeneralizedLinearRegressionFormula.Distribution.Normal)
    val estBeta = estimator.estimate(MatSample(x), MatSample(y1))
    assertArrayEquals(Array(2.93425072514275, 7.015427560018422, 1.1781502495819423),
      estBeta.parameters.toArray, 1e-10)
  }

  @Test
  def normal2(): Unit ={
    val formula = new GeneralizedLinearRegressionFormula(
      ColVecOption(None, None, 1),
      GeneralizedLinearRegressionFormula.Distribution.Normal)
    val estimator = GeneralizedLinearRegression(formula)
    val estBeta = estimator.estimate(MatSample(x), MatSample(y1))
    assertArrayEquals(Array(2.925929022118332, 7.027721781090618),
      estBeta.parameters.toArray, 1e-10)
  }

  @Test
  def normal3(): Unit ={
    val formula = new GeneralizedLinearRegressionFormula(
      ColVecOption(None, 7, 1),
      GeneralizedLinearRegressionFormula.Distribution.Normal)
    val estimator = GeneralizedLinearRegression(formula)
    val estBeta = estimator.estimate(MatSample(x), MatSample(y1))
    assertArrayEquals(Array(2.9268868511851296),
      estBeta.parameters.toArray, 1e-10)
  }

  @Test
  def binomial1(): Unit ={
    val formula = new GeneralizedLinearRegressionFormula(
      ColVecOption(None, None, None),
      GeneralizedLinearRegressionFormula.Distribution.Binomial)
    val estimator = GeneralizedLinearRegression(formula)
    val estBeta = estimator.estimate(MatSample(x), MatSample(y2))
    assertArrayEquals(Array(2.1725446013296352, 5.1064282562249526, 0.660639294277851),
      estBeta.parameters.toArray, 1e-10)
  }

  @Test
  def binomial2(): Unit ={
    val formula = new GeneralizedLinearRegressionFormula(
      ColVecOption(None, None, 1),
      GeneralizedLinearRegressionFormula.Distribution.Binomial)
    val estimator = GeneralizedLinearRegression(formula)
    val estBeta = estimator.estimate(MatSample(x), MatSample(y2))
    assertArrayEquals(Array(2.36235816959322, 5.476662037873774),
      estBeta.parameters.toArray, 1e-10)
  }

  @Test
  def binomial3(): Unit ={
    val formula = new GeneralizedLinearRegressionFormula(
      ColVecOption(None, 7, 1),
      GeneralizedLinearRegressionFormula.Distribution.Binomial)
    val estimator = GeneralizedLinearRegression(formula)
    val estBeta = estimator.estimate(MatSample(x), MatSample(y2))
    assertArrayEquals(Array(2.9223170610613383),
      estBeta.parameters.toArray, 1e-10)
  }

  @Test
  def poisson(): Unit = {
    val formula = new GeneralizedLinearRegressionFormula(
      ColVecOption(None, None, None),
      GeneralizedLinearRegressionFormula.Distribution.Poisson)
    val estimator = GeneralizedLinearRegression(formula)
    val estBeta = estimator.estimate(MatSample(x), MatSample(y3))
    println(estBeta.parameters)
  }
}
