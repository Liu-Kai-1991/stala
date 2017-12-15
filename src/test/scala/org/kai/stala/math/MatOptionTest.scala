package org.kai.stala.math

import org.junit.Test

import scala.util.Try

class MatOptionTest {
  val d1: MatOption = MatOption((3, None), (None, 4))
  val d2: MatOption = MatOption((Some(3), None, Some(4)))
  val d3: MatOption = MatOption(None)

  val rv: RowVecOption = RowVecOption(3, None, 4)
  val cv: ColVecOption = ColVecOption(3, None, 4)

  @Test
  def fillNA(): Unit = {
    assert(Try(d1.toMat(Seq(1.0))).toOption.isEmpty)
    assert(d1.toMat(Seq(1.0, 2.0)).equalValue(Mat((3, 1), (2, 4))))
    assert(d2.toMat(Seq(1.0)).equalValue(rv.toMat(Seq(1.0))))
    assert(d3.toMat(3).equalValue(Mat(3)))
    assert(cv.toMat(3).to1DVector == rv.toMat(3).to1DVector)
    assert(d1.toMat(2.0).equalValue(Mat((3, 2), (2, 4))))
  }
}
