package org.kai.stala.stala.est

import org.apache.commons.math3.distribution.BinomialDistribution
import org.junit.Test
import org.kai.stala.math.{ColVec, ColVecOption, Mat}
import org.kai.stala.stat.est.MatSample
import org.kai.stala.stat.est.impl.{GeneralizedLinearRegression, GeneralizedLinearRegressionFormula, LinearRegressionMLE, LinearRegressionMLEFormula}

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

  @Test
  def normal1(): Unit ={
    val formula = new GeneralizedLinearRegressionFormula(
      ColVecOption(None, None, None),
      GeneralizedLinearRegressionFormula.Distribution.Normal)
    val estimator = GeneralizedLinearRegression(formula)
    val estBeta = estimator.estimate(MatSample(x), MatSample(y1))
    println(estBeta)
  }

  @Test
  def normal2(): Unit ={
    val formula = new GeneralizedLinearRegressionFormula(
      ColVecOption(None, None, 1),
      GeneralizedLinearRegressionFormula.Distribution.Normal)
    val estimator = GeneralizedLinearRegression(formula)
    val estBeta = estimator.estimate(MatSample(x), MatSample(y1))
    println(estBeta)
  }

  @Test
  def normal3(): Unit ={
    val formula = new GeneralizedLinearRegressionFormula(
      ColVecOption(None, 7, 1),
      GeneralizedLinearRegressionFormula.Distribution.Normal)
    val estimator = GeneralizedLinearRegression(formula)
    val estBeta = estimator.estimate(MatSample(x), MatSample(y1))
    println(estBeta)
  }

  @Test
  def binomial1(): Unit ={
    val formula = new GeneralizedLinearRegressionFormula(
      ColVecOption(None, None, None),
      GeneralizedLinearRegressionFormula.Distribution.Binomial)
    val estimator = GeneralizedLinearRegression(formula)
    val estBeta = estimator.estimate(MatSample(x), MatSample(y2))
    println(estBeta)
  }

  @Test
  def binomial2(): Unit ={
    val formula = new GeneralizedLinearRegressionFormula(
      ColVecOption(None, None, 1),
      GeneralizedLinearRegressionFormula.Distribution.Binomial)
    val estimator = GeneralizedLinearRegression(formula)
    val estBeta = estimator.estimate(MatSample(x), MatSample(y2))
    println(estBeta)
  }

  @Test
  def binomial3(): Unit ={
    val formula = new GeneralizedLinearRegressionFormula(
      ColVecOption(None, 7, 1),
      GeneralizedLinearRegressionFormula.Distribution.Binomial)
    val estimator = GeneralizedLinearRegression(formula)
    val estBeta = estimator.estimate(MatSample(x), MatSample(y2))
    println(estBeta)
  }
}
