package org.kai.stala.math

import org.apache.commons.math3.linear.{MatrixUtils, RealMatrix, RealVector}

import scala.collection.SeqView
import scala.collection.immutable._
import scala.reflect.ClassTag

trait Vec extends Mat {
  def length: Int = to1DVector.size
  def apply(i: Int): Double = to1DVector(i)

  private var realVectorOption: Option[RealVector] = None
  protected def getRealVector: RealVector = MatrixUtils.createRealVector(to1DVector.toArray)
  def realVector: RealVector =
    if (realVectorOption.isDefined) realVectorOption.get
    else {
      realVectorOption = Some(getRealVector)
      realVectorOption.get
    }
}

class RowVec(override val to1DVector: Vector[Double]) extends Vec {
  lazy val height: Int = 1
  lazy val width: Int = to1DVector.length

  override def to2DVector: Vector[Vector[Double]] = Vector(to1DVector)

  override def apply(i: Int, j: Int): Double = {
    require(i == 0)
    to1DVector(j)
  }

  override def map(f: Double => Double): RowVec = RowVec(to1DVector.map(f))

  override def * (that: Mat): Mat = {
    require(width == that.height, "DenseMat: Matrix dimension must compile")
    that match {
      case d: DenseMat =>
        val rm = realMatrix.multiply(that.realMatrix)
        RowVec(rm.getRow(0).toVector)
      case cv: ColVec =>
        Mat((to1DVector, cv.to1DVector).zipped.map(_*_).sum)
    }
  }

  override def * (that: Double): Mat =
    if (that == 1.0) this else {
      new RowVec(to1DVector.map(_ * that))
    }

  override def + (that: Mat): Mat = {
    require(dim == that.dim, "DenseMat: Matrix dimension must compile")
    that match {
      case rv: RowVec =>
        new RowVec((to1DVector, rv.to1DVector).zipped.map(_ + _))
    }
  }

  override def - (that: Mat): Mat = {
    require(dim == that.dim, "DenseMat: Matrix dimension must compile")
    that match {
      case rv: RowVec =>
        new RowVec((to1DVector, rv.to1DVector).zipped.map(_ - _))
    }
  }

  override def cBind(that: Mat): Mat = {
    require(height == that.height)
    that match {
      case d: DenseMat =>
        throw new IllegalArgumentException("row vector can not cbind with matrix")
      case cv: ColVec =>
        throw new IllegalArgumentException("row vector can not cbind with col vector")
      case rv: RowVec =>
        RowVec(to1DVector ++ rv.to1DVector)
    }
  }

  override def rBind(that: Mat): Mat = {
    require(width == that.width)
    that match {
      case d: DenseMat =>
        DenseMat(to1DVector +: d.m)
      case cv: ColVec =>
        throw new IllegalArgumentException("row vector can not rbind with col vector")
      case rv: RowVec =>
        DenseMat(Vector(to1DVector, rv.to1DVector))
    }
  }
}

object RowVec{
  class RowVecJ(
    override val to1DVector : Vector[Double],
    override val realVector : RealVector
  ) extends RowVec(to1DVector)

  def apply(v: Vector[Double]): RowVec = new RowVec(v)
  def apply[T:ClassTag](v: T*)(implicit n: Numeric[T]): RowVec = new RowVec(v.toVector.map(n.toDouble))
  def apply[T:ClassTag](v: Iterable[T])(implicit n: Numeric[T]): RowVec = new RowVec(v.toVector.map(n.toDouble))
  def apply(v: RealVector): RowVec = new RowVecJ(v.toArray.toVector, v)
  def fill(n: Int, v: Double): RowVec = new RowVec(Vector.fill[Double](n)(v))
}

class ColVec(override val to1DVector: Vector[Double]) extends Vec {
  lazy val height: Int = to1DVector.length
  lazy val width: Int = 1

  override def to2DVector: Vector[Vector[Double]] = Vector(to1DVector).transpose

  override def apply(i: Int, j: Int): Double = {
    require(j == 0)
    to1DVector(i)
  }

  override def map(f: Double => Double): ColVec = ColVec(to1DVector.map(f))

  override def * (that: Mat): Mat = {
    require(width == that.height, "DenseMat: Matrix dimension must compile")
    that match {
      case d: DenseMat =>
        val rm = realMatrix.multiply(d.realMatrix)
        DenseMat(rm)
      case rv: RowVec =>
        val rm = realMatrix.multiply(rv.realMatrix)
        DenseMat(rm)
    }
  }

  override def * (that: Double): Mat =
    if (that == 1.0) this else {
      new ColVec(to1DVector.map(_ * that))
    }

  override def + (that: Mat): Mat = {
    require(dim == that.dim, "DenseMat: Matrix dimension must compile")
    that match {
      case rv: ColVec =>
        new ColVec((to1DVector, rv.to1DVector).zipped.map(_ + _))
    }
  }

  override def - (that: Mat): Mat = {
    require(dim == that.dim, "DenseMat: Matrix dimension must compile")
    that match {
      case rv: ColVec =>
        new ColVec((to1DVector, rv.to1DVector).zipped.map(_ - _))
    }
  }

  override def cBind(that: Mat): Mat = {
    require(height == that.height)
    that match {
      case d: DenseMat =>
        DenseMat((to1DVector, d.m).zipped.map{
          case (l,r) => l +: r
        })
      case cv: ColVec =>
        DenseMat((to1DVector, cv.to1DVector).zipped.map{
          case (l,r) => Vector(l,r)
        })
      case rv: RowVec =>
        throw new IllegalArgumentException("Col vector can not cbind with row vector")
    }
  }

  override def rBind(that: Mat): Mat = {
    require(width == that.width)
    that match {
      case d: DenseMat =>
        throw new IllegalArgumentException("Col vector can not rbind with matrix")
      case cv: ColVec =>
        ColVec(to1DVector ++ cv.to1DVector)
      case rv: RowVec =>
        throw new IllegalArgumentException("Col vector can not rbind with row vector")
    }
  }

  override def colSeqs: SeqView[Vector[Double], scala.Seq[_]] = Seq(to1DVector).view
  override def cols: SeqView[ColVec, scala.Seq[_]] = Seq(this).view
}

object ColVec{
  class ColVecJ(
    override val to1DVector: Vector[Double],
    override val realVector: RealVector
  ) extends ColVec(to1DVector)

  def apply(v: Vector[Double]): ColVec = new ColVec(v)
  def apply[T:ClassTag](v: T*)(implicit n: Numeric[T]): ColVec = new ColVec(v.toVector.map(n.toDouble))
  def apply[T:ClassTag](v: Iterable[T])(implicit n: Numeric[T]): ColVec = new ColVec(v.toVector.map(n.toDouble))
  def apply(v: RealVector): ColVec = new ColVecJ(v.toArray.toVector, v)
  def fill(n: Int, v: Double): ColVec = new ColVec(Vector.fill[Double](n)(v))
}