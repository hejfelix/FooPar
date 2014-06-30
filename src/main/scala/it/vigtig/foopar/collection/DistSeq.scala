package it.vigtig.foopar.collection

import scala.collection.mutable.ArrayBuffer
import scala.collection.generic.CanBuildFrom
import it.vigtig.foopar.comm.pure.FooParGroup
import it.vigtig.foopar.FooParApp
import scala.collection.mutable.Builder

object DistSeq {

  def apply[T](ts: T*)(implicit fpapp: FooParApp) = fromSeq(ts)

  protected[foopar] def fromSeq[T](ts: Seq[T], group: Option[FooParGroup] = None)(implicit fpapp: FooParApp): DistSeq[T] = {
    val g: FooParGroup = if (group.isDefined) group.get else fpapp.getGroup(ts.size)
    val part = if (ts.isEmpty || !g.ranks.contains(fpapp.globalRank)) None else Some(ts(g.localRank % ts.size))
    new DistSeq(part, g, ts.size)
  }

  implicit def canBuildFrom[T, U](implicit fpapp: FooParApp): CanBuildFrom[DistSeq[T], U, DistSeq[U]] =
    new CanBuildFrom[DistSeq[T], U, DistSeq[U]] {
      def apply(): Builder[U, DistSeq[U]] = newBuilder(None, 0)
      def apply(from: DistSeq[T]): Builder[U, DistSeq[U]] = newBuilder(Some(from.group), from.size)
    }
  def newBuilder[T](group: Option[FooParGroup], size: Int)(implicit fpapp: FooParApp): Builder[T, DistSeq[T]] =
    group match {
      case Some(g) => new DistTraversableBuilder[T] mapResult (t => new DistSeq(arrayToHead(t), g, size))
      case None => new DistTraversableBuilder[T] mapResult (t => DistSeq(t(0)))
    }
  def arrayToHead[T](a: ArrayBuffer[T]) = if (a.isEmpty) None else Some(a.head)

}

class DistSeq[+T](val elem: Option[T], val group: FooParGroup, val size: Int)(implicit val fpapp: FooParApp)
  extends DistTraversableLike[T, DistSeq[T]]
  with TraversableNumOps[T, DistSeq[T]] 


