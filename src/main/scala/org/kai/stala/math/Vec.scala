package org.kai.stala.math

import org.apache.commons.math3.linear.{MatrixUtils, RealMatrix, RealVector}

import scala.collection.SeqView
import scala.collection.immutable._
import scala.reflect.ClassTag

trait Vec extends Mat {
  def toArray: Array[Double]
  lazy val realVector: RealVector =
    MatrixUtils.createRealVector(toArray)
}

case class RowVec(private val v: Array[Double]) extends Vec {
  lazy val height: Int = 1
  lazy val width: Int = v.length

  override def toArray: Array[Double] = v
  override def to2DArray: Array[Array[Double]] = Array(v)

  def apply(i: Int): Double = v(i)

  override def apply(i: Int, j: Int): Double = {
    require(i == 0)
    v(j)
  }

  override def * (that: Mat): Mat = {
    require(width == that.height, "DenseMat: Matrix dimension must compile")
    that match {
      case d: DenseMat =>
        val res = Array.ofDim[Double](d.width)
        var i = 0
        var j = 0
        var sum = 0.0
        while (j < d.width){
          sum = 0.0
          i = 0
          while (i < width){
            sum += d(i, j) * v(i)
            i+=1
          }
          res(j) = sum
          j+=1
        }
        new RowVec(res)
      case cv: ColVec =>
        Mat((v, cv.toArray).zipped.map(_*_).sum)
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
  def apply[T:ClassTag](v: T*)(implicit n: Numeric[T]): RowVec = new RowVec(v.toArray.map(n.toDouble))
  def apply[T:ClassTag](v: Iterable[T])(implicit n: Numeric[T]): RowVec = new RowVec(v.toArray.map(n.toDouble))
  def apply(v: RealVector): RowVec = new RowVec(v.toArray)
}

case class ColVec(private val v: Array[Double]) extends Vec {
  lazy val height: Int = v.length
  lazy val width: Int = 1

  def apply(idx: Int): Double = v(idx)

  override def toArray: Array[Double] = v

  override def to2DArray: Array[Array[Double]] = Array(v).transpose

  override def apply(i: Int, j: Int): Double = {
    require(j == 0)
    v(i)
  }

  override def * (that: Mat): Mat = {
    require(width == that.height, "DenseMat: Matrix dimension must compile")
    that match {
      case d: DenseMat =>
        val res = Array.ofDim[Double](height, d.width)
        val row = d.to2DArray.head
        var i = 0
        var j = 0
        var x = 0.0
        while (i < height){
          x = v(i)
          j = 0
          while (j < d.width){
            res(i)(j) = x * row(j)
            j+=1
          }
          i+=1
        }
        DenseMat.createUnsafe(res)
      case rv: RowVec =>
        val res = Array.ofDim[Double](height, rv.width)
        var i = 0
        var j = 0
        var x = 0.0
        while (i < height){
          x = v(i)
          j = 0
          while (j < rv.width){
            res(i)(j) = x * rv(j)
            j+=1
          }
          i+=1
        }
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

  override def colSeqs: SeqView[Array[Double], scala.Seq[_]] = Seq(v).view
  override def cols: SeqView[ColVec, scala.Seq[_]] = Seq(this).view
}

object ColVec{
  def apply[T:ClassTag](v: T*)(implicit n: Numeric[T]): ColVec = new ColVec(v.toArray.map(n.toDouble))
  def apply[T:ClassTag](v: Iterable[T])(implicit n: Numeric[T]): ColVec = new ColVec(v.toArray.map(n.toDouble))
  def apply(v: RealVector): ColVec = new ColVec(v.toArray)
}