package org.kai.stala.stala.est

import org.junit.Test
import org.kai.stala.math.{ColVec, Mat}
import org.kai.stala.stat.est.impl.{LinearRegressionMLE, LinearRegressionMLEFormula}

import scala.util.Random

class LinearRegressionMLETest {

  @Test
  def testRegression(): Unit ={
    val formula = new LinearRegressionMLEFormula(2)
    val estimator = new LinearRegressionMLE(formula)
  }
}
