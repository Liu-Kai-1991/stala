package org.kai.stala.math

trait VecOption extends MatOption{
  def length: Int = to1DOptionVector.size
  def apply(i: Int): Option[Double] = to1DOptionVector(i)
  def toVec(fill: Double): Vec
  def toVec(fills: Iterable[Double]): Vec

  override def toMat(fill: Double): Mat = toVec(fill)

  override def toMat(fills: Iterable[Double]): Mat = toVec(fills)

  override lazy val numberOfNone: Int = to1DOptionVector.count(_.isEmpty)
}

class RowVecOption(override val to1DOptionVector: Vector[Option[Double]]) extends VecOption {
  override def apply(i: Int, j: Int): Option[Double] = {
    require(i == 0)
    apply(j)
  }
  override def to2DOptionVector: Vector[Vector[Option[Double]]] = Vector(to1DOptionVector)

  override def toVec(fill: Double): RowVec = RowVec(to1DOptionVector.map(_.getOrElse(fill)))

  override def toVec(fills: Iterable[Double]): RowVec = {
    require(numberOfNone == fills.size)
    val fillsIter = fills.iterator
    RowVec(to1DOptionVector.map(_.getOrElse(fillsIter.next())))
  }

  def height: Int = 1
  override lazy val width: Int = to1DOptionVector.length
}

object RowVecOption{
  def apply(v: Vector[Option[Double]]): RowVecOption = new RowVecOption(v)
  def apply(v: Any*): RowVecOption = new RowVecOption(v.toVector.map(MatOption.doubleOption))
}

class ColVecOption(override val to1DOptionVector: Vector[Option[Double]]) extends VecOption {
  override def apply(i: Int, j: Int): Option[Double] = {
    require(j == 0)
    apply(i)
  }
  override def to2DOptionVector: Vector[Vector[Option[Double]]] = Vector(to1DOptionVector).transpose

  override def toVec(fill: Double): ColVec = ColVec(to1DOptionVector.map(_.getOrElse(fill)))

  override def toVec(fills: Iterable[Double]): ColVec = {
    require(numberOfNone == fills.size)
    val fillsIter = fills.iterator
    ColVec(to1DOptionVector.map(_.getOrElse(fillsIter.next())))
  }

  def width: Int = 1
  override lazy val height: Int = to1DOptionVector.length
}

object ColVecOption{
  def apply(v: Vector[Option[Double]]): ColVecOption = new ColVecOption(v)
  def apply(v: Any*): ColVecOption = new ColVecOption(v.toVector.map(MatOption.doubleOption))
}