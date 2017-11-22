package object util {

  implicit class IterableImplict[T](val x: Iterable[T]) {
    def singleDistinct: T = {
      val set = x.toSet
      require(set.size == 1, s"singleDistinct failed, has ${set.size} unique elements")
      set.head
    }

    def single: Boolean =
      x.toSet.size == 1
  }

}
