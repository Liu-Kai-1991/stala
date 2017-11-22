package math

import util._

import org.apache.commons.math3.linear.{MatrixUtils, RealMatrix}

trait Mat extends Iterable[Double] {
  def apply(i: Int, j: Int): Double

  def dim: (Int, Int)

  def realMatrix: RealMatrix

  def printMat(): Unit = {
    val header = s"${this.getClass.getSimpleName}: ${dim._1} * ${dim._2}"
    println(header)
    println(header.map(_ => "=").mkString)
    this.grouped(dim._2).foreach{
      row => println(row.mkString(" "))
    }
  }

  def * (that: Mat): Mat = {
    DenseMat(realMatrix.multiply(that.realMatrix))
  }
}

class DenseMat protected(val m: Vector[Vector[Double]]) extends Mat {
  def apply(i: Int, j: Int): Double = m(i)(j)

  def realMatrix: RealMatrix = MatrixUtils.createRealMatrix(m.map(_.toArray).toArray)

  override def dim: (Int, Int) = (m.size, m.map(_.size).singleDistinct)

  override def iterator: Iterator[Double] = m.flatten.toIterator
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
    new DenseMatJ(realMatrix.getData.toVector.map(_.toVector), realMatrix)
  }
}