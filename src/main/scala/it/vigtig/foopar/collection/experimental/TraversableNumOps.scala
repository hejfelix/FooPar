package it.vigtig.foopar.collection.experimental


import scala.collection.generic.CanBuildFrom
import Numeric.Implicits._
import Ordering.Implicits._

trait TraversableNumOps[+T, +Repr] extends DistTraversableLike[T, Repr] {
  self: Repr =>
  private[this] def min[U >: T](a: U, b: U)(implicit num: Numeric[U]) =
    if (num.compare(a, b) <= 0) a else b
  private[this] def max[U >: T](a: U, b: U)(implicit num: Numeric[U]) =
    if (num.compare(a, b) > 0) a else b

  def maxD[U >: T](implicit num: Numeric[U]): Option[U] =
    reduceD[U](max)
  def minD[U >: T](implicit num: Numeric[U]): Option[U] =
    reduceD[U](min)
  def sumD[U >: T](implicit num: Numeric[U]): Option[U] =
    reduceD[U](_ + _)
  def productD[U >: T](implicit num: Numeric[U]): Option[U] =
    reduceD[U](_ * _)

  def allMaxD[U >: T](implicit num: Numeric[U]): Option[U] =
    allReduceD[U](max)
  def allMinD[U >: T](implicit num: Numeric[U]): Option[U] =
    allReduceD[U](min)
  def allSumD[U >: T](implicit num: Numeric[U]): Option[U] =
    allReduceD[U](_ + _)
  def allProductD[U >: T](implicit num: Numeric[U]): Option[U] =
    allReduceD[U](_ * _)

  def avgD[U >: T](implicit num: Numeric[U]): Option[Double] =
    sumD[U].map(num.toDouble).map(_ / size.toDouble)
  def allAvgD[U >: T](implicit num: Numeric[U]): Option[Double] =
    allSumD[U].map(num.toDouble).map(_ / size.toDouble)

}