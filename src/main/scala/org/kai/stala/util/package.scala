package org.kai.stala

import java.lang.{ Byte => JByte, Double => JDouble, Float => JFloat, Integer => JInt, Long => JLong, Short => JShort }

import org.apache.commons.math3.linear.{ MatrixUtils, RealMatrix }

import scala.reflect.{ ClassTag, classTag }

package object util {
  implicit class IterableImplict[T](val x: Iterable[T]) {
    def singleDistinct: T = {
      val set = x.toSet
      require(set.size == 1, s"singleDistinct failed, has ${ set.size } unique elements")
      set.head
    }

    def single: Boolean =
      x.toSet.size == 1

    def cross[K](y: Iterable[K]): Iterable[(T, K)] =
      for (a <- x.view; b <- y) yield (a, b)
  }

  implicit class ArrayImplict[T](val x: Array[T]) {
    def singleDistinct: T = {
      val set = x.toSet
      require(set.size == 1, s"singleDistinct failed, has ${ set.size } unique elements")
      set.head
    }

    def single: Boolean =
      x.toSet.size == 1

    def cross[K](y: Iterable[K]): Iterable[(T, K)] =
      for (a <- x.view; b <- y) yield (a, b)
  }

  def time[R](block: => R): (R, Long) = {
    val t0 = System.nanoTime()
    val result = block
    (result, System.nanoTime() - t0)
  }

  private val scalaDouble: Class[_] = classOf[Double]
  private val scalaInt: Class[_] = classOf[Int]
  private val scalaLong: Class[_] = classOf[Long]
  private val scalaByte: Class[_] = classOf[Byte]
  private val scalaShort: Class[_] = classOf[Short]
  private val scalaFloat: Class[_] = classOf[Float]

  private val javaDouble: Class[_ <: Comparable[_]] = classOf[JDouble]
  private val javaInt: Class[_ <: Comparable[_]] = classOf[JInt]
  private val javaLong: Class[_ <: Comparable[_]] = classOf[JLong]
  private val javaShort: Class[_ <: Comparable[_]] = classOf[JShort]
  private val javaByte: Class[_ <: Comparable[_]] = classOf[JByte]
  private val javaFloat: Class[_ <: Comparable[_]] = classOf[JFloat]

  def javaClass(c: Class[_]): Class[_ <: Comparable[_]] = c match {
    case `scalaDouble` => javaDouble
    case `scalaInt` => javaInt
    case `scalaLong` => javaLong
    case `scalaByte` => javaByte
    case `scalaFloat` => javaFloat
    case `scalaShort` => javaShort
  }

  def toJavaType[T: ClassTag](xs: Iterable[T]): Iterable[_ <: Comparable[_]] = classTag[T].runtimeClass match {
    case `scalaDouble` => xs.map(x => JDouble.valueOf(x.asInstanceOf[Double]))
    case `scalaInt` => xs.map(x => JInt.valueOf(x.asInstanceOf[Int]))
    case `scalaLong` => xs.map(x => JLong.valueOf(x.asInstanceOf[Long]))
    case `scalaByte` => xs.map(x => JByte.valueOf(x.asInstanceOf[Byte]))
    case `scalaFloat` => xs.map(x => JFloat.valueOf(x.asInstanceOf[Float]))
    case `scalaShort` => xs.map(x => JShort.valueOf(x.asInstanceOf[Short]))
  }

  trait CachedRTFunction

  case class CachedRTFunction0D[To](f: () => To) extends CachedRTFunction {
    private var toOption: Option[To] = None
    def apply(): To =
      if (toOption.isDefined) toOption.get
      else {
        toOption = Some(f())
        toOption.get
      }
  }
}
