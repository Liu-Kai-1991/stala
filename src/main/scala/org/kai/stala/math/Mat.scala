package org.kai.stala.math

import org.apache.commons.math3.linear.{MatrixUtils, RealMatrix}

import scala.collection.immutable._
import org.kai.stala.util._

import scala.collection.SeqView
import scala.reflect.ClassTag

trait Mat{
  def apply(i: Int, j: Int): Double
  def height: Int
  def width: Int
  def dim: (Int, Int) = (height, width)
  def to2DArray: Array[Array[Double]]

  def printMat(): Unit = {
    val header = s"${this.getClass.getSimpleName}: ${dim._1} * ${dim._2}"
    println(header)
    println(header.map(_ => "=").mkString)
    to2DArray.foreach{
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
  protected def getRealMatrix: RealMatrix = MatrixUtils.createRealMatrix(to2DArray)
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

  def colSeqs: SeqView[Array[Double], collection.Seq[_]] = Range(0, width).view.map(i => to2DArray.map(_(i)))
  def cols: SeqView[ColVec, collection.Seq[_]] = colSeqs.map(ColVec.apply)
  def rows: SeqView[RowVec, Array[_]] = to2DArray.view.map(RowVec.apply)
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
  def apply(v: Double): Mat = DenseMat.createUnsafe(Array(Array(v)))
  def apply(m: Double*)(implicit i: DummyImplicit): Mat = DenseMat.createUnsafe(Array(m.toArray))
  def apply(m: Product*): Mat = {
    val arrarr = m.toArray.map(_.productIterator.toArray.map(doubleValue))
    DenseMat.createSafe(arrarr)
  }
}

class DenseMat protected(protected val m: Array[Array[Double]]) extends Mat {
  def apply(i: Int, j: Int): Double = m(i)(j)

  override lazy val height: Int = m.length
  override lazy val width: Int = m.head.length
  override def to2DArray: Array[Array[Double]] = m

  override protected def getRealMatrix: RealMatrix = MatrixUtils.createRealMatrix(m)

  override def * (that: Mat): Mat = {
    require(width == that.height, "DenseMat: Matrix dimension must compile")
    that match {
      case d: DenseMat =>
        if (height * d.width > 4096) DenseMat(realMatrix.multiply(d.realMatrix)) else {
          val res = Array.ofDim[Double](height, d.width)
          val col = Array.ofDim[Double](width)
          var j = 0
          var k = 0
          var sum = 0.0
          var row = Array.emptyDoubleArray
          while (j < d.width){
            k = 0
            while (k < width){
              col(k) = d.m(k)(j)
              k+=1
            }
            var i = 0
            while (i < height){
              row = m(i)
              sum = 0.0
              k = 0
              while (k < width) {
                sum += col(k)*row(k)
                k+=1
              }
              res(i)(j) = sum
              i+=1
            }
            j+=1
          }
          new DenseMat(res)
        }
      case rv: RowVec =>
        val res = Array.ofDim[Double](height, that.width)
        var i = 0
        var j = 0
        var row = 0.0
        while (i < height){
          row = m(i)(0)
          j = 0
          while (j < rv.width){
            res(i)(j) = row * rv(j)
            j+=1
          }
          i+=1
        }
        new DenseMat(res)
      case cv: ColVec =>
        val res = Array.ofDim[Double](height)
        var i = 0
        var j = 0
        var sum = 0.0
        var row = Array.emptyDoubleArray
        while (i < height){
          row = m(i)
          sum = 0.0
          j = 0
          while (j < width){
            sum+=row(j)*cv(j)
            j+=1
          }
          res(i) = sum
          i+=1
        }
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
        new RowVec((m.head, rv.toArray).zipped.map(_ + _) )
      case cv: ColVec =>
        new ColVec((colSeqs.head, cv.toArray).zipped.map(_ + _) )
    }
  }
}

object DenseMat {
  class DenseMatJ(
    override protected val m: Array[Array[Double]],
    override val realMatrix: RealMatrix
  ) extends DenseMat(m)

  def apply[T:ClassTag](m: Iterable[Iterable[T]])(implicit n: Numeric[T]): DenseMat = {
    require(m.map(_.size).single)
    new DenseMat(m.toArray.map(_.toArray.map(n.toDouble)))
  }

  def createSafe[T:ClassTag](m: Array[Array[T]])(implicit n: Numeric[T]): DenseMat = {
    require(m.map(_.length).single)
    new DenseMat(m.map(_.map(n.toDouble)))
  }

  def apply(realMatrix: RealMatrix) : DenseMatJ = {
    new DenseMatJ(realMatrix.getData, realMatrix.copy)
  }

  def createUnsafe(m: Array[Array[Double]]): DenseMat = new DenseMat(m)
}