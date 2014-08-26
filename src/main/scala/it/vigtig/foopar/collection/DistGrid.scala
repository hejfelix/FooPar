package it.vigtig.foopar.collection

import it.vigtig.foopar.comm.pure.FooParGroup
import it.vigtig.foopar.FooParApp
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Builder
import scala.collection.mutable.HashMap

case class Ranges(group: FooParGroup, ranges: Range*) {
  val dimSizes = ranges.map(_.size)
  val size = dimSizes.product
  val bases = dimSizes.scanRight(1)(_ * _).tail
  val offsets = ranges.map(_.start)

  def rankBaseMap(rank: Int): IndexedSeq[Int] = {
    assert(rank <= size, rank + " > " + size)
    (bases.toIndexedSeq, dimSizes, offsets)
      .zipped
      .map(rank / _ % _ + _)
  }

  def baseRankMap(crds: Seq[Int]): Int = {
    assert(crds.size == ranges.length)
    (bases, crds, offsets)
      .zipped
      .map((b, c, o) => b * (c - o))
      .sum
  }

  def seqGroup(varDim: Int) = {
    val mycrds = rankBaseMap(group.localRank)
    val seqCoords = for (x <- ranges(varDim)) yield mycrds.patch(varDim, Seq(x), 1)
    val ranks = seqCoords.map(baseRankMap)
    group.subGroup(ranks.toArray)
  }
  lazy val xGroup = seqGroup(1)
  lazy val yGroup = seqGroup(0)
  lazy val zGroup = seqGroup(2)

  val seqRankMap: HashMap[Int, IndexedSeq[Int]] = new HashMap[Int, IndexedSeq[Int]]()
}
object DistGrid {
  def apply(rs: Range*)(implicit fpapp: FooParApp) = {
    val grp = fpapp.getGroup(rs.map(_.size).product)
    val ranges = Ranges(grp, rs: _*)
    val myCoords: Option[List[Int]] =
      if (grp.partOfGroup) Some(ranges.rankBaseMap(grp.localRank).toList) else None
    new DistGrid[Seq[Int]](myCoords, grp, ranges)
  }

  implicit def canBuildFrom[T, U](implicit fpapp: FooParApp): CanBuildFrom[DistGrid[T], U, DistGrid[U]] =
    new CanBuildFrom[DistGrid[T], U, DistGrid[U]] {
      def apply(): Builder[U, DistGrid[U]] = ???
      def apply(from: DistGrid[T]): Builder[U, DistGrid[U]] = newBuilder(Some(from.group), from.ranges)
    }
  def newBuilder[T](group: Option[FooParGroup], rs: Ranges)(implicit fpapp: FooParApp): Builder[T, DistGrid[T]] =
    group match {
      case Some(g) => new DistTraversableBuilder[T] mapResult (t => new DistGrid(arrayToHead(t), g, rs))
    }
  def arrayToHead[T](a: ArrayBuffer[T]) = if (a.isEmpty) None else Some(a(0))

}

class DistGrid[T](val elem: Option[T], val group: FooParGroup, val ranges: Ranges)(implicit fpapp: FooParApp)
  extends DistTraversableLike[T, DistGrid[T]]
  with TraversableNumOps[T, DistGrid[T]] {

  def size = ranges.size
  def apply(s: Seq[Int]): Option[T] = {
    val root = if (group.partOfGroup) ranges.baseRankMap(s) else 0
    apply(root)
  }

  def getSeq(varDim: Int): DistSeq[T] = {
    val grp = ranges.seqGroup(varDim)
    new DistSeq(elem, grp, grp.size)
  }

  lazy val xs0: DistSeq[T] = new DistSeq(elem, ranges.xGroup, ranges.xGroup.size)
  lazy val xs1: DistSeq[T] = new DistSeq(elem, ranges.yGroup, ranges.yGroup.size)
  lazy val xs2: DistSeq[T] = new DistSeq(elem, ranges.zGroup, ranges.zGroup.size)
  
  lazy val xs: DistSeq[T] = xs0
  lazy val ys: DistSeq[T] = xs1
  lazy val zs: DistSeq[T] = xs2
}
