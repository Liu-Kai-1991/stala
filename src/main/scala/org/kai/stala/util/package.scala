package org.kai.stala

package object util {

  implicit class IterableImplict[T](val x: Iterable[T]) {
    def singleDistinct: T = {
      val set = x.toSet
      require(set.size == 1, s"singleDistinct failed, has ${set.size} unique elements")
      set.head
    }

    def single: Boolean =
      x.toSet.size == 1

    def cross[K](y: Iterable[K]): Iterable[(T,K)] =
      for (a <- x.view; b <- y) yield (a,b)
  }

  def time[R](block: => R): (R, Long) = {
    val t0 = System.nanoTime()
    val result = block
    (result, System.nanoTime() - t0)
  }
}
