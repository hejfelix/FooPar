package it.vigtig.foopar.collection.experimental

import scala.collection.mutable.ArrayBuffer
import scala.collection.generic.CanBuildFrom
import it.vigtig.foopar.comm.pure.FooParGroup
import it.vigtig.foopar.FooParApp
import scala.collection.mutable.Builder

object DistSeq {

  def apply[T](ts: T*)(implicit fpapp: FooParApp) = fromSeq(ts)
  def apply[T](n: Int)(part: => Seq[T])(implicit fpapp: FooParApp) = fromParts(n)(part)
  def apply(rng: Range, p: Int = -1)(implicit fpapp: FooParApp) = ranged(rng, p)
  protected[foopar] def fromSeq[T](ts: Seq[T], group: Option[FooParGroup] = None)(implicit fpapp: FooParApp): DistSeq[T] = {
    val g: FooParGroup = if (group.isDefined) group.get else fpapp.getGroup(ts.size)
    val part = if (ts.isEmpty || !g.ranks.contains(fpapp.globalRank)) Nil else Seq(ts(g.localRank % ts.size))
    new DistSeq(part, g, ts.size)
  }

  implicit def canBuildFrom[T, U](implicit fpapp: FooParApp): CanBuildFrom[DistSeq[T], U, DistSeq[U]] =
    new CanBuildFrom[DistSeq[T], U, DistSeq[U]] {
      def apply(): Builder[U, DistSeq[U]] = newBuilder(None, 0)
      def apply(from: DistSeq[T]): Builder[U, DistSeq[U]] = newBuilder(Some(from.group), from.size)
    }
  def newBuilder[T](group: Option[FooParGroup], size: Int)(implicit fpapp: FooParApp): Builder[T, DistSeq[T]] =
    group match {
      case Some(g) => new ArrayBuffer[T] mapResult (t => new DistSeq(t, g, size))
      case None => new ArrayBuffer[T] mapResult (t => DistSeq(t(0)))
    }
  def arrayToHead[T](a: ArrayBuffer[T]) = if (a.isEmpty) Nil else Seq(a.head)

  def fromParts[T](n: Int)(part: => Seq[T])(implicit fpapp: FooParApp): DistSeq[T] = {
    val group = fpapp.getGroup(n)
    val prt = if (!group.partOfGroup) Nil else part
    new DistSeq[T](prt, group, n)
  }

  def ranged(rng: Range, p: Int = -1)(implicit fpapp: FooParApp) = {
    val pp = if (p == -1) fpapp.worldSize else p
    val (from, to) = (rng.start, rng.end)
    val size = if (to <= from) 0 else to - from
    val partition = size / pp //floor
    val group = fpapp.getGroup(pp)
    val r = group.localRank
    val part =
      if (group.isLastRank)
        (from + r * partition) to to
      else
        (from + r * partition) until from + (r + 1) * partition
    new DistSeq(if (group.partOfGroup) part else Nil, group, size)
  }

}

class DistSeq[+T](val part: Seq[T], val group: FooParGroup, val size: Int)(implicit val fpapp: FooParApp)
  extends DistTraversableLike[T, DistSeq[T]]
  with TraversableNumOps[T, DistSeq[T]] 
