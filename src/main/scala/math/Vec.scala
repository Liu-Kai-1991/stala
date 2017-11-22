package math
import org.apache.commons.math3.linear.{MatrixUtils, RealMatrix}

trait Vec extends Mat {
  lazy val realMatrix: RealMatrix =
    if (dim._1 == 1) MatrixUtils.createRealMatrix(Array(this.toArray))
    else MatrixUtils.createRealMatrix(this.toArray.map(d => Array(d)))
}

case class RowVec(v: Vector[Double]) extends Vec {
  def dim: (Int, Int) = (1, v.size)

  override def apply(i: Int, j: Int): Double = {
    require(i == 1)
    v(j)
  }

  override def iterator: Iterator[Double] = v.iterator
}

case class ColVec(v: Vector[Double]) extends Vec {
  def dim: (Int, Int) = (v.size, 1)

  override def apply(i: Int, j: Int): Double = {
    require(j == 1)
    v(i)
  }

  override def iterator: Iterator[Double] = v.iterator
}

object Test extends App{
  val x = DenseMat(Vector(Vector(1,2), Vector(3,4)))
  val y = RowVec(Vector(1,2))
  val z = ColVec(Vector(1,2))
  x.printMat()
  y.printMat()
  z.printMat()
  (x * z).printMat()
  (y * x).printMat()
  (y * z).printMat()
  (z * y).printMat()
}