package org.kai.stala.stala.est

import org.junit.Test
import org.kai.stala.math.{ColVec, Mat}
import org.kai.stala.stat.est.MatSample
import org.kai.stala.stat.est.impl.{LinearRegressionMLE, LinearRegressionMLEFormula}

import scala.util.Random

class LinearRegressionMLETest {

  @Test
  def testRegression(): Unit ={
    val formula = new LinearRegressionMLEFormula(2)
    val estimator = new LinearRegressionMLE(formula)
    val x = Mat.from1DVector(50, 2, Range(0, 100).map(_ => Random.nextDouble()))
    val beta = ColVec(3,7)
    val y = x * beta + ColVec(Range(0, 50).map(_ => Random.nextDouble()))
    val estBeta = estimator.estimate(MatSample(x), MatSample(y))
    println(estBeta)
  }
}
