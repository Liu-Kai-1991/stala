package org.kai.stala.math

trait MatOption {
  def apply(i: Int, j: Int): Option[Double]
  def height: Int
  def width: Int
  def dim: (Int, Int) = (height, width)
  def to2DOptionVector: Vector[Vector[Option[Double]]]
  def to1DOptionVector: Vector[Option[Double]] = to2DOptionVector.flatten
  def numberOfNone: Int
  def toMat(fills: Iterable[Double]): Mat
  def toMat(fill: Double): Mat
  def toMat(fillsIter: Iterator[Double]): Mat

  def printMat(): Unit = {
    val header = s"${ this.getClass.getSimpleName }: ${ dim._1 } * ${ dim._2 }"
    println(header)
    println(header.map(_ => "=").mkString)
    to2DOptionVector.foreach{
      row =>
        println(row.map{
          case None => "None"
          case Some(d) => d.toString
        }.mkString(" "))
    }
  }

  override def toString: String = {
    s"${ this.getClass.getSimpleName }" +
      s"(${
        Range(0, height).map(i => Range(0, width).map(j => apply(i, j)).map{
          case None => "None"
          case Some(d) => d.toString
        }.mkString(", ")).mkString("; ")
      })"
  }
}

class DenseMatOption(m: Vector[Vector[Option[Double]]]) extends MatOption {
  override lazy val height: Int = m.length
  override lazy val width: Int = m.head.length
  override lazy val numberOfNone: Int = m.map(_.count(_.isEmpty)).sum

  override def apply(i: Int, j: Int): Option[Double] = m(i)(j)

  override def to2DOptionVector: Vector[Vector[Option[Double]]] = m

  override def toMat(fill: Double): Mat =
    DenseMat(m.map(_.map(_.getOrElse(fill))))

  override def toMat(fills: Iterable[Double]): Mat = {
    require(numberOfNone == fills.size, "numberOfNone should equal to sequence length")
    val fillsIter = fills.iterator
    DenseMat(m.map(_.map(_.getOrElse(fillsIter.next()))))
  }

  override def toMat(fillsIter: Iterator[Double]): Mat = {
    DenseMat(m.map(_.map(_.getOrElse(fillsIter.next()))))
  }
}

object DenseMatOption {
  def apply(m: DenseMat): DenseMatOption = new DenseMatOption(m.to2DVector.map(_.map(Some.apply)))
}

object MatOption {
  def doubleOption(x: Any): Option[Double] = x match {
    case Some(b) => Some(Mat.doubleValue(b))
    case b: Double => Some(b)
    case b: Int => Some(b.toDouble)
    case b: Byte => Some(b.toDouble)
    case b: Short => Some(b.toDouble)
    case b: Long => Some(b.toDouble)
    case b: Float => Some(b.toDouble)
    case None => None
    // Forbid Char to be casted as Double. case b: Char=> b.toDouble
    case nonNumeric => throw new IllegalArgumentException(s"Non numeric type in Mat.apply: $nonNumeric")
  }
  val classProduct: Class[_] = classOf[Product]
  val classDoubleOption: Class[_] = classOf[Option[Double]]

  def apply(v: Option[Double]): MatOption = new DenseMatOption(Vector(Vector(v)))
  def apply(m: Product*): MatOption = {
    val arrarr = m.toVector.map(_.productIterator.toVector.map(doubleOption))
    new DenseMatOption(arrarr)
  }

  def empty(height: Int, width: Int): MatOption =
    new DenseMatOption(Vector.fill[Vector[Option[Double]]](height)(Vector.fill[Option[Double]](width)(None)))

  def apply(m: Mat): MatOption = m match {
    case d: DenseMat => new DenseMatOption(m.to2DVector.map(_.map(Some.apply)))
    case rv: RowVec => new RowVecOption(rv.to1DVector.map(Some.apply))
    case cv: ColVec => new ColVecOption(cv.to1DVector.map(Some.apply))
  }
}