package org.kai.stala.math

import org.apache.commons.math3.linear.{MatrixUtils, RealMatrix, RealVector}
import scala.collection.immutable._

trait Vec extends Mat {
  lazy val realVector: RealVector =
    MatrixUtils.createRealVector(this.flatten.toArray)
}

case class RowVec(v: IndexedSeq[Double]) extends Vec {
  lazy val height: Int = 1
  lazy val width: Int = v.size

  override def apply(idx: Int): IndexedSeq[Double] = v

  override def apply(i: Int, j: Int): Double = {
    require(i == 0)
    v(j)
  }
}

object RowVec{
  def apply[T](v: T*)(implicit n: Numeric[T]): RowVec = new RowVec(v.toVector.map(n.toDouble))
  def apply[T](v: Iterable[T])(implicit n: Numeric[T]): RowVec = new RowVec(v.toVector.map(n.toDouble))
  def apply(v: RealVector): RowVec = new RowVec(v.toArray.toVector)
}

case class ColVec(v: IndexedSeq[Double]) extends Vec {
  lazy val height: Int = v.size
  lazy val width: Int = 1

  override def apply(idx: Int): IndexedSeq[Double] = IndexedSeq(v(idx))

  override def apply(i: Int, j: Int): Double = {
    require(j == 0)
    v(i)
  }
}

object ColVec{
  def apply[T](v: T*)(implicit n: Numeric[T]): ColVec = new ColVec(v.toVector.map(n.toDouble))
  def apply[T](v: Iterable[T])(implicit n: Numeric[T]): ColVec = new ColVec(v.toVector.map(n.toDouble))
  def apply(v: RealVector): ColVec = new ColVec(v.toArray.toVector)
}