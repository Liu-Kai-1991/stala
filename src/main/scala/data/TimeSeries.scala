package data

import java.time.temporal.Temporal

trait TimeSeries[T] extends IndexedSeq[T] {
  def timeStamp: IndexedSeq[Temporal]
  def data: IndexedSeq[T]
  override def toString: String = s"${getClass.getSimpleName}(${timeStamp.zip(data).map(_.toString).mkString(",")})"
  override def iterator: Iterator[T] = data.iterator
  override def apply(idx: Int): T = data(idx)
  override def length: Int = timeStamp.length
}

case class VectorTimeSeries[T] private(
  timeStamp: Vector[Temporal],
  data: Vector[T]
) extends TimeSeries[T]

object VectorTimeSeries{
  def apply[T](
    timeStamp: Iterable[Temporal],
    data     : Iterable[T]
  ): VectorTimeSeries[T] = {
    require(timeStamp.size == data.size, "TimeStamp should has same size as data")
    new VectorTimeSeries(timeStamp.toVector, data.toVector)
  }
}