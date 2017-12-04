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

  def * (that: Mat): Mat
  def * (that: Double): Mat
  def + (that: Mat): Mat

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

  def rowSeqs: SeqView[IndexedSeq[Double], collection.Seq[_]] = this.view
  def colSeqs: SeqView[IndexedSeq[Double], collection.Seq[_]] = Range(0, width).view.map(i => this.map(_(i)))
  def rows: SeqView[RowVec, collection.Seq[_]] = rowSeqs.map(RowVec.apply)
  def cols: SeqView[ColVec, collection.Seq[_]] = colSeqs.map(ColVec.apply)
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
  def apply(m: Double*)(implicit i: DummyImplicit): Mat =
    RowVec(m.toVector)
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

  override def iterator: Iterator[IndexedSeq[Double]] = m.iterator

  override protected def getRealMatrix: RealMatrix = MatrixUtils.createRealMatrix(m.map(_.toArray).toArray)

  override def * (that: Mat): Mat = {
    require(width == that.height, "DenseMat: Matrix dimension must compile")
    that match {
      case d: DenseMat =>
        val res =
          (for (col <- d.colSeqs) yield
            (for (row <- rowSeqs) yield
              (col, row).zipped.map(_ * _).sum).toVector).toVector
        new DenseMat(res.transpose)
      case rv: RowVec =>
        val res =
          for (d <- rv.v) yield
            for (r <- colSeqs.head) yield
              d*r
        new DenseMat(res)
      case cv: ColVec =>
        val res = rowSeqs.map(row => (row, cv.v).zipped.map(_*_).sum).toVector
        new ColVec(res)
    }
  }

  override def * (that: Double): Mat =
    if (that == 1.0) this else {
      new DenseMat(m.map(_.map(_ * that)))
    }

  override def + (that: Mat): Mat = {
    require(dim == that.dim, "DenseMat: Matrix dimension must compile")
    that match {
      case d: DenseMat =>
        val res = (m, d.m).zipped.map{
          case (row, thatRow) =>
            (row, thatRow).zipped.map(_ + _)
        }
        new DenseMat(res)
      case rv: RowVec =>
        new RowVec((rowSeqs.head, rv.v).zipped.map(_ + _) )
      case cv: ColVec =>
        new ColVec((colSeqs.head, cv.v).zipped.map(_ + _) )
    }
  }
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