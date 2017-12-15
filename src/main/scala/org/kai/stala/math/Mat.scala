package org.kai.stala.math

import org.apache.commons.math3.linear.{ LUDecomposition, MatrixUtils, RealMatrix }

import scala.collection.immutable._
import org.kai.stala.util._

import scala.collection.SeqView
import scala.reflect.ClassTag

trait Mat {
  def apply(i: Int, j: Int): Double
  def height: Int
  def width: Int
  def dim: (Int, Int) = (height, width)
  def to2DVector: Vector[Vector[Double]]
  def to1DVector: Vector[Double] = to2DVector.flatten

  def printMat(): Unit = {
    val header = s"${ this.getClass.getSimpleName }: ${ dim._1 } * ${ dim._2 }"
    println(header)
    println(header.map(_ => "=").mkString)
    to2DVector.foreach{
      row => println(row.mkString(" "))
    }
  }

  override def toString: String = {
    s"${ this.getClass.getSimpleName }" +
      s"(${ Range(0, height).map(i => Range(0, width).map(j => apply(i, j)).mkString(", ")).mkString("; ") })"
  }

  def *(that: Mat): Mat
  def *(that: Double): Mat
  def +(that: Mat): Mat
  def -(that: Mat): Mat

  val realMatrixCached: CachedRTFunction0D[RealMatrix] =
    CachedRTFunction0D(() => MatrixUtils.createRealMatrix(to2DVector.toArray.map(_.toArray)))
  def realMatrix: RealMatrix = realMatrixCached()

  def isSquare: Boolean = dim._1 == dim._2

  def equalValue(obj: Any): Boolean = obj match {
    case m: Mat =>
      dim == m.dim &&
        (Range(0, dim._1) cross Range(0, dim._2)).forall{
          case (i, j) => apply(i, j) == m(i, j)
        }
    case d: Double =>
      dim == (1, 1) && apply(0, 0) == d
    case _ => super.equals(obj)
  }
  def colSeqs: SeqView[Vector[Double], collection.Seq[_]] = Range(0, width).view.map(i => to2DVector.map(_ (i)))
  def cols: SeqView[ColVec, collection.Seq[_]] = colSeqs.map(ColVec.apply)
  def rows: SeqView[RowVec, collection.Seq[_]] = to2DVector.view.map(RowVec.apply)

  def cBind(that: Mat): Mat
  def rBind(that: Mat): Mat
  def map(f: Double => Double): Mat

  val lUDecomposition: CachedRTFunction0D[LUDecomposition] = CachedRTFunction0D(() => new LUDecomposition(realMatrix))
  def determinant: Double = lUDecomposition().getDeterminant
  def inverse: Mat
  def transpose: Mat
}

object Mat {
  def doubleValue(x: Any): Double = x match {
    case b: Double => b
    case b: Int => b.toDouble
    case b: Byte => b.toDouble
    case b: Short => b.toDouble
    case b: Long => b.toDouble
    case b: Float => b.toDouble
    // Forbid Char to be casted as Double. case b: Char=> b.toDouble
    case nonNumeric => throw new IllegalArgumentException(s"Non numeric type in Mat.apply: $nonNumeric")
  }
  def apply(v: Double): Mat = DenseMat.createUnsafe(Vector(Vector(v)))
  def apply(m: Double*)(implicit i: DummyImplicit): Mat = DenseMat.createUnsafe(Vector(m.toVector))
  def apply(m: Product*): Mat = {
    val arrarr = m.toVector.map(_.productIterator.toVector.map(doubleValue))
    DenseMat.createSafe(arrarr)
  }

  def from1DVector(height: Int, width: Int, v: Seq[Double]): Mat = {
    assert(v.size == height * width)
    DenseMat.createUnsafe(v.grouped(width).map(_.toVector).toVector)
  }

  def applyRowVec(rvs: Iterable[RowVec]): Mat = DenseMat(rvs.toVector.map(_.to1DVector))

  def applyColVec(cvs: Iterable[ColVec]): Mat = DenseMat(cvs.toVector.map(_.to1DVector).transpose)
}

case object DummyMat extends Mat {
  override def *(that: Double): Mat = ???
  override def *(that: Mat): Mat = ???
  override def +(that: Mat): Mat = ???
  override def -(that: Mat): Mat = ???
  override def apply(i: Int, j: Int): Double = ???
  override def cBind(that: Mat): Mat = ???
  override def cols: SeqView[ColVec, scala.Seq[_]] = ???
  override def colSeqs: SeqView[Vector[Double], scala.Seq[_]] = ???
  override def height: Int = 0
  override def width: Int = 0
  override def inverse: Mat = ???
  override def map(f: Double => Double): Mat = ???
  override def to2DVector: Vector[Vector[Double]] = ???
  override def rBind(that: Mat): Mat = ???
  override def transpose: Mat = ???
}

class DenseMat protected(val m: Vector[Vector[Double]]) extends Mat {
  def apply(i: Int, j: Int): Double = m(i)(j)

  override lazy val height: Int = m.length
  override lazy val width: Int = m.head.length
  override def to2DVector: Vector[Vector[Double]] = m

  override def map(f: Double => Double): DenseMat = DenseMat(m.map(_.map(f)))

  override def *(that: Mat): Mat = {
    require(width == that.height, "DenseMat: Matrix dimension must compile")
    that match {
      case d: DenseMat =>
        DenseMat(realMatrix.multiply(d.realMatrix))
      case rv: RowVec =>
        DenseMat(realMatrix.multiply(rv.realMatrix))
      case cv: ColVec =>
        val rm = realMatrix.multiply(cv.realMatrix)
        new ColVec(rm.getColumn(0).toVector)
    }
  }

  override def *(that: Double): Mat =
    if (that == 1.0) this else {
      new DenseMat(m.map(_.map(_ * that)))
    }

  override def +(that: Mat): Mat = {
    require(dim == that.dim, "DenseMat: Matrix dimension must compile")
    that match {
      case d: DenseMat =>
        val res = (m, d.m).zipped.map{
          case (row, thatRow) =>
            (row, thatRow).zipped.map(_ + _)
        }
        new DenseMat(res)
      case rv: RowVec =>
        new RowVec((m.head, rv.to1DVector).zipped.map(_ + _))
      case cv: ColVec =>
        new ColVec((colSeqs.head, cv.to1DVector).zipped.map(_ + _))
    }
  }

  override def -(that: Mat): Mat = {
    require(dim == that.dim, "DenseMat: Matrix dimension must compile")
    that match {
      case d: DenseMat =>
        val res = (m, d.m).zipped.map{
          case (row, thatRow) =>
            (row, thatRow).zipped.map(_ - _)
        }
        new DenseMat(res)
      case rv: RowVec =>
        new RowVec((m.head, rv.to1DVector).zipped.map(_ - _))
      case cv: ColVec =>
        new ColVec((colSeqs.head, cv.to1DVector).zipped.map(_ - _))
    }
  }

  override def cBind(that: Mat): Mat = {
    require(height == that.height)
    that match {
      case d: DenseMat =>
        DenseMat((m, d.m).zipped.map{
          case (l, r) => l ++ r
        })
      case cv: ColVec =>
        DenseMat((m, cv.to1DVector).zipped.map{
          case (l, r) => l :+ r
        })
      case rv: RowVec =>
        throw new IllegalArgumentException("Matrix can not cbind with row vector")
    }
  }

  override def rBind(that: Mat): Mat = {
    require(width == that.width)
    that match {
      case d: DenseMat =>
        DenseMat(m ++ d.m)
      case cv: ColVec =>
        throw new IllegalArgumentException("Matrix can not rbind with col vector")
      case rv: RowVec =>
        DenseMat(m :+ rv.to1DVector)
    }
  }

  override def inverse: Mat =
    DenseMat(lUDecomposition().getSolver.getInverse)

  override def transpose: Mat = {
    DenseMat(m.transpose)
  }
}

object DenseMat {
  class DenseMatJ(
    override val m: Vector[Vector[Double]],
    override val realMatrix: RealMatrix
  ) extends DenseMat(m)

  def apply[T: ClassTag](m: Iterable[Iterable[T]])(implicit n: Numeric[T]): DenseMat = {
    require(m.map(_.size).single)
    new DenseMat(m.toVector.map(_.toVector.map(n.toDouble)))
  }

  def createSafe[T: ClassTag](m: Vector[Vector[T]])(implicit n: Numeric[T]): DenseMat = {
    require(m.map(_.length).single)
    new DenseMat(m.map(_.map(n.toDouble)))
  }

  def apply(realMatrix: RealMatrix): DenseMatJ = {
    new DenseMatJ(realMatrix.getData.map(_.toVector).toVector, realMatrix.copy)
  }

  def createUnsafe(m: Vector[Vector[Double]]): DenseMat = new DenseMat(m)
}