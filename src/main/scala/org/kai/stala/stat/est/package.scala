package org.kai.stala.stat

package object est {

  def mean(x: Iterable[Double]): Double = x.sum / x.size

  def variance(x: Iterable[Double]): Double = {
    val meanValue = mean(x)
    x.map(t => (t - meanValue) * (t - meanValue)).sum / (x.size - 1)
  }

  def std(x: Iterable[Double]): Double = math.sqrt(variance(x))

}
