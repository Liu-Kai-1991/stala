package org.kai.stala.math

import org.junit.Test
import org.junit.Assert.assertTrue

class MatTest{
  val x = Mat((1,2),(3,4))
  val k = Mat((1,1), (1,1))
  val y = RowVec(1,2)
  val z = ColVec(1,2)

  @Test
  def construct(): Unit = {
    assertTrue(RowVec(1,2).equalValue(RowVec(Vector(1,2))))
    assertTrue(ColVec(1,2).equalValue(ColVec(Vector(1,2))))
    assertTrue(Mat(5).equalValue(5.0))
    assertTrue(Mat(2,3).equalValue(DenseMat(Vector(Vector(2,3)))))
    assertTrue(Mat(2,3).equalValue(RowVec(2,3)))
    assertTrue(Mat(Tuple1(2), Tuple1(3)).equalValue(DenseMat(Vector(Vector(2), Vector(3)))))
    assertTrue(Mat(Tuple1(2), Tuple1(3)).equalValue(ColVec(2,3)))
    assertTrue(x.rows.head.equalValue(RowVec(1,2)))
    assertTrue(x.cols.head.equalValue(ColVec(1,3)))
  }

  @Test
  def multiplication(): Unit = {
    (x * z).printMat()
    (y * x).printMat()
    (y * z).printMat()
    (z * y).printMat()
    assertTrue((x * z).equalValue(ColVec(5, 11)))
    assertTrue((y * x).equalValue(RowVec(7, 10)))
    assertTrue((y * z).equalValue(5.0))
    assertTrue((z * y).equalValue(Mat((1,2), (2,4))))
    assertTrue((x * 2).equalValue(Mat((2,4),(6,8))))
    assertTrue((y * 2).equalValue(RowVec(2,4)))
  }

  @Test
  def plus(): Unit = {
    (x + k).printMat()
    assertTrue((x + k).equalValue(Mat((2,3), (4,5))))
  }
}