package it.vigtig.foopar.collection.experimental

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Builder

object DistTraversableLike {
  implicit class Zipped[T1, Repr1, T2, Repr2](tup: (DistTraversableLike[T1, Repr1], DistTraversableLike[T2, Repr2])) {
    def zipWithD[U, That](f: (T1, T2) => U)(implicit bf: CanBuildFrom[Repr1, U, That]) = tup._1.zipWithD(f)(tup._2)
  }
}

trait DistTraversableLike[+T, +Repr]
  extends DistTraversable[T] {
  //  with Function1[Int, Option[T]] {
  self: Repr =>

  def foreach(f: T => Unit) { part.foreach(f) }
  def size: Int

  def liftOrIdentity[T](f: (T, T) => T) = (a: Option[T], b: Option[T]) =>
    (a, b) match {
      case (Some(a), None) => Some(a)
      case (None, Some(b)) => Some(b)
      case (Some(a), Some(b)) => Some(f(a, b))
      case (None, None) => None
    }

  def mapD[B, That](f: T => B)(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    val b: Builder[B, That] = bf(this)
    if (group.partOfGroup)
      for (x <- this) b += f(x)
    b.result
  }

  def filterD[That](p: T => Boolean)(implicit bf: CanBuildFrom[Repr, T, That]): That = {
    val b: Builder[T, That] = bf(this)
    for (x <- part if p(x)) b += x
    b.result
  }

  def flatMapD[B, That](f: T => Traversable[B])(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    val b: Builder[B, That] = bf(this)
    if (group.partOfGroup)
      for (x <- this) b ++= f(x)
    b.result
  }

  def reduceD[T1 >: T](assOp: (T1, T1) => T1): Option[T1] = {
    val reduced = if (part.isEmpty) None else Some(part.reduce(assOp))
    if (group.partOfGroup)
      group.allOneReduceSeq(reduced, liftOrIdentity(assOp)).foreach(return _)
    return None
  }

  def allReduceD[T1 >: T](assOp: (T1, T1) => T1): Option[T1] = {
    val reduced = if (part.isEmpty) None else Some(part.reduce(assOp))
    if (group.partOfGroup)
      group.allReduce(reduced, liftOrIdentity(assOp))
    else
      None
  }

  def scan1lD[T1 >: T, That](assOp: (T1, T1) => T1)(implicit bf: CanBuildFrom[Repr, T1, That]): That = {
    val b = bf(this)
    for (x <- group.prefixSumSeq(part, assOp))
      b += x
    b.result
  }

  def scanD[B >: T, That](z: B)(assOp: (B, B) ⇒ B)(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    val b = bf(this)
    val ppart = if (group.localRank == 0) z +: part else part
    if (group.partOfGroup)
      for (x <- group.prefixSumSeq(ppart, assOp))
        b += x
    b.result
  }

  def zipD[B, That](that: DistTraversable[B])(implicit bf: CanBuildFrom[Repr, (T, B), That]) = {
    val b = bf(this)
    val newSize = math.min(size, that.group.size)
    val zipped: Seq[(T, B)] = if (that.group.partOfGroup)
      group.zip[T, B](that.part, that.group)
    else if (group.partOfGroup)
      group.zip[T, B](part, that.group)
    else
      Nil
    zipped foreach b.+=
    b.result
  }

  def zipWithD[B, That, U](f: (T, B) => U)(that: DistTraversable[B])(implicit bf: CanBuildFrom[Repr, U, That]) = {
    val b = bf(this)
    val newSize = math.min(size, that.group.size)
    val zipped: Seq[(T, B)] = if (that.group.partOfGroup)
      group.zip[T, B](that.part, that.group)
    else if (group.partOfGroup)
      group.zip[T, B](part, that.group)
    else
      Nil
    zipped.map { case (a, b) => f(a, b) } foreach b.+=
    b.result
  }

}