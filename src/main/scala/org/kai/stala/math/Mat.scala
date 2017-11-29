package org.kai.stala.math

import org.apache.commons.math3.linear.{MatrixUtils, RealMatrix}

import scala.collection.immutable._
import scala.collection
import org.kai.stala.util._

import scala.collection.SeqView

trait Mat extends IndexedSeq[IndexedSeq[Double]] {
  def apply(i: Int, j: Int): Double
  def height: Int
  def width: Int
  def dim: (Int, Int) = (height, width)
  override def length: Int = height

  def printMat(): Unit = {
    val header = s"${this.getClass.getSimpleName}: ${dim._1} * ${dim._2}"
    println(header)
    println(header.map(_ => "=").mkString)
    this.foreach{
      row => println(row.mkString(" "))
    }
  }

  override def toString: String = {
    s"${this.getClass.getSimpleName}" +
      s"(${Range(0, height).map(i => Range(0, width).map(j => apply(i, j)).mkString(", ")).mkString("; ")})"
  }

  def * (that: Mat): Mat = {
    DenseMat(realMatrix.multiply(that.realMatrix))
  }

  def * (that: Double): Mat = {
    DenseMat.createUnsafe(this.map(_.map(_ * that)))
  }

  def + (that: Mat): Mat = {
    require(dim == that.dim)
    val res = (this, that).zipped.map{
      case (row, thatRow) =>
        (row, thatRow).zipped.map(_ + _)
    }
    DenseMat.createUnsafe(res)
  }

  private var realMatrixOption: Option[RealMatrix] = None
  protected def getRealMatrix: RealMatrix = MatrixUtils.createRealMatrix(this.map(_.toArray).toArray)
  def realMatrix: RealMatrix =
    if (realMatrixOption.isDefined) realMatrixOption.get
    else {
      realMatrixOption = Some(getRealMatrix)
      realMatrixOption.get
    }

  def isSquare: Boolean = dim._1 == dim._2

  def equalValue(obj: Any): Boolean = obj match {
    case m: Mat =>
      dim == m.dim &&
        (Range(0, dim._1) cross Range(0, dim._2)).forall{
          case (i,j) => apply(i,j) == m(i,j)
        }
    case d: Double =>
      dim == (1,1) && apply(0,0) == d
    case _ => super.equals(obj)
  }

  def rows: SeqView[RowVec, collection.Seq[_]] = this.view.map(RowVec.apply)
  def cols: SeqView[ColVec, collection.Seq[_]] = Range(0, width).view.map(i => ColVec(this.map(_(i))))
}

object Mat{
  def doubleValue(x: Any): Double = x match {
    case b: Double => b
    case b: Int=> b.toDouble
    case b: Byte => b.toDouble
    case b: Short => b.toDouble
    case b: Long=> b.toDouble
    case b: Float=> b.toDouble
    // Forbid Char to be casted as Double. case b: Char=> b.toDouble
    case nonNumeric => throw new IllegalArgumentException(s"Non numeric type in Mat.apply: $nonNumeric")
  }
  def apply(v: Double): Mat = DenseMat(Vector(Vector(v)))
  def apply(m: Double*)(implicit i: DummyImplicit): Mat = {
    val vecvec = Vector(m.toVector.map(doubleValue))
    DenseMat(vecvec)
  }
  def apply(m: Product*): Mat = {
    val vecvec = m.toVector.map(_.productIterator.toVector.map(doubleValue))
    DenseMat(vecvec)
  }
}

class DenseMat protected(val m: IndexedSeq[IndexedSeq[Double]]) extends Mat {
  def apply(i: Int, j: Int): Double = m(i)(j)

  override lazy val height: Int = m.length
  override lazy val width: Int = m.head.size

  override def apply(idx: Int): IndexedSeq[Double] = m(idx)

  override protected def getRealMatrix: RealMatrix = MatrixUtils.createRealMatrix(m.map(_.toArray).toArray)
}

object DenseMat {
  class DenseMatJ(
    override val m: Vector[Vector[Double]],
    override val realMatrix: RealMatrix
  ) extends DenseMat(m)

  def apply[T](m: Iterable[Iterable[T]])(implicit n: Numeric[T]): DenseMat = {
    require(m.map(_.size).single)
    new DenseMat(m.toVector.map(_.toVector.map(n.toDouble)))
  }

  def apply(realMatrix: RealMatrix) : DenseMatJ = {
    new DenseMatJ(realMatrix.getData.toVector.map(_.toVector), realMatrix.copy)
  }

  def createUnsafe(m: IndexedSeq[IndexedSeq[Double]]): DenseMat = new DenseMat(m)
}