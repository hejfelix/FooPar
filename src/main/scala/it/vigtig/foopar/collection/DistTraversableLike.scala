package it.vigtig.foopar.collection

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Builder
import scala.collection.mutable.ArrayBuffer
import it.vigtig.foopar.FooParApp
import it.vigtig.foopar.comm.pure.FooParGroup

object DistTraversableLike {
  def apply[T](elem: T)(implicit fpapp: FooParApp) = DistVal(Some(elem), fpapp.globalGroup)
  def arrayToHead[T](a: ArrayBuffer[T]) = if (a.isEmpty) None else Some(a.head)
}
trait DistTraversableLike[+T, +Repr]
  extends DistTraversable[T]
  with Function1[Int, Option[T]] {
  self: Repr =>

  def foreach(f: T => Unit) { elem.foreach(f) }
  def size: Int

  def liftOrIdentity[T](f: (T, T) => T) = (a: Option[T], b: Option[T]) =>
    a ++ b reduceOption f

  def filterD[That](p: T => Boolean)(implicit bf: CanBuildFrom[Repr, T, That]): That = {
    val b: Builder[T, That] = bf(this)
    elem.filter(p).foreach(x => b += x)
    b.result
  }

  def mapD[B, That](f: T => B)(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    val b: Builder[B, That] = bf(this)
    if (group.partOfGroup)
      for (x <- this) b += f(x)
    b.result
  }

  def shiftD[That](delta: Int)(implicit bf: CanBuildFrom[Repr, T, That]): That = {
    val b: Builder[T, That] = bf(this)
    if (group.partOfGroup)
      for (x <- this) b += group.shift[T](x, delta)
    b.result
  }

  def reduceD[T1 >: T](assOp: (T1, T1) => T1): Option[T1] =
    if (group.partOfGroup)
      group.fpReduce(elem, liftOrIdentity(assOp)).flatten
    else
      None

  def countD(p: T => Boolean): Option[Long] =
    if (group.partOfGroup) {
      val contribution = elem.map(x => if (p(x)) 1l else 0l)
      group.fpReduce(contribution, liftOrIdentity((a: Long, b: Long) => a + b), 0).flatten
    } else
      None

      
  def maxByD[B](f: T => B)(implicit cmp: Ordering[B]) =
    if (group.partOfGroup) {
      def max(a: T, b: T) = if (cmp.compare(f(a), f(b)) >= 0) a else b
      group.fpReduce(elem, liftOrIdentity(max), 0).flatten
    } else
      None

  def existsD(p: T => Boolean): Option[Boolean] =
    if (group.partOfGroup) {
      val contribution = elem.map(x => if (p(x)) true else false)
      group.fpReduce(contribution, liftOrIdentity((a: Boolean, b: Boolean) => a || b), 0).flatten
    } else
      None

  def forallD(p: T => Boolean): Option[Boolean] = existsD(x => !p(x)).map(!_)

  def scan1D[T1 >: T](assOp: (T1, T1) => T1): Option[T1] =
    if (group.partOfGroup)
      group.prefixSum(elem, liftOrIdentity(assOp))
    else
      None

  def allReduceD[T1 >: T](assOp: (T1, T1) => T1): Option[T1] =
    if (group.partOfGroup)
      group.allReduce(elem, liftOrIdentity(assOp))
    else
      None

  def allToAllBC[T1 >: T]: Option[ArrayBuffer[T1]] = {
    if (group.partOfGroup) {
      for (x <- this) {
        return Some(group.allToAll(x))
      }
    }
    None
  }

  def apply(root: Int): Option[T] =
    if (group.partOfGroup)
      group.fpOneToAll(elem, root % group.size)
    else
      None

  def align = if (group.partOfGroup) group.barrier

}