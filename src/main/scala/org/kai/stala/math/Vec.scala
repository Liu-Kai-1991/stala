package org.kai.stala.math

import org.apache.commons.math3.linear.{MatrixUtils, RealMatrix, RealVector}

import scala.collection.SeqView
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

  override def * (that: Mat): Mat = {
    require(width == that.height, "DenseMat: Matrix dimension must compile")
    that match {
      case d: DenseMat =>
        val res =
          (for (col <- d.colSeqs) yield
              (v, col).zipped.map(_ * _).sum).toVector
        new RowVec(res)
      case cv: ColVec =>
        DenseMat(Vector(Vector((v, cv.v).zipped.map(_*_).sum)))
    }
  }

  override def * (that: Double): Mat =
    if (that == 1.0) this else {
      new RowVec(v.map(_ * that))
    }

  override def + (that: Mat): Mat = {
    require(dim == that.dim, "DenseMat: Matrix dimension must compile")
    that match {
      case rv: RowVec =>
        new RowVec((v, rv.v).zipped.map(_ + _))
    }
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

  override def * (that: Mat): Mat = {
    require(width == that.height, "DenseMat: Matrix dimension must compile")
    that match {
      case d: DenseMat =>
        val res =
          for (c <- v) yield
            for (r <- d.m.head) yield c*r
        DenseMat.createUnsafe(res)
      case rv: RowVec =>
        val res =
          for (c <- v) yield
            for (r <- rv.v) yield c*r
        DenseMat.createUnsafe(res)
    }
  }

  override def * (that: Double): Mat =
    if (that == 1.0) this else {
      new ColVec(v.map(_ * that))
    }

  override def + (that: Mat): Mat = {
    require(dim == that.dim, "DenseMat: Matrix dimension must compile")
    that match {
      case rv: ColVec =>
        new ColVec((v, rv.v).zipped.map(_ + _))
    }
  }

  override def colSeqs: SeqView[IndexedSeq[Double], scala.Seq[_]] = Seq(v).view
  override def cols: SeqView[ColVec, scala.Seq[_]] = Seq(this).view
}

object ColVec{
  def apply[T](v: T*)(implicit n: Numeric[T]): ColVec = new ColVec(v.toVector.map(n.toDouble))
  def apply[T](v: Iterable[T])(implicit n: Numeric[T]): ColVec = new ColVec(v.toVector.map(n.toDouble))
  def apply(v: RealVector): ColVec = new ColVec(v.toArray.toVector)
}