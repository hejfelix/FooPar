package it.vigtig.foopar.collection

import scala.collection.mutable.ArrayBuffer
import scala.collection.generic.CanBuildFrom
import it.vigtig.foopar.comm.pure.FooParGroup
import it.vigtig.foopar.FooParApp
import scala.collection.mutable.Builder

object DistGrid3D {
//  def apply(r1: Range, r2: Range, r3: Range)(implicit fpapp: FooParApp) = {
//    val ranges = Ranges(r1, r2, r3)
//    val grp = fpapp.getGroup(ranges.size)
//    val myCoords: Option[(Int, Int, Int)] =
//      if (grp.partOfGroup) {
//        val Vector(x, y, z) = ranges.rankBaseMap(grp.localRank).toSeq
//        Some((x, y, z))
//      } else {
//        None
//      }
//    new DistGrid3D(myCoords, grp, ranges)
//  }

  //  implicit def canBuildFrom[T, U](implicit fpapp: FooParApp): CanBuildFrom[DistGrid3D[T], U, DistGrid3D[U]] =
  //    new CanBuildFrom[DistGrid3D[T], U, DistGrid3D[U]] {
  //      def apply(): Builder[U, DistGrid3D[U]] = ???
  //      def apply(from: DistGrid3D[T]): Builder[U, DistGrid3D[U]] = newBuilder(Some(from.group), from.ranges)
  //    }
  //  def newBuilder[T](group: Option[FooParGroup], rs: Ranges)(implicit fpapp: FooParApp): Builder[T, DistGrid3D[T]] =
  //    group match {
  //      case Some(g) => new DistTraversableBuilder[T] mapResult (t => new DistGrid3D(arrayToHead(t), g, rs))
  //    }
  //  def arrayToHead[T](a: ArrayBuffer[T]) = if (a.isEmpty) None else Some(a.head)

}

class DistGrid3D[T](elem: Option[T], group: FooParGroup, ranges: Ranges)(implicit fpapp: FooParApp)
  extends DistGrid[T](elem, group, ranges)(fpapp) {
  private def coords: Option[Seq[Int]] =
    if (group.partOfGroup)
      Some(ranges.rankBaseMap(group.localRank))
    else
      None
  def tup = coords.map(_.toList).map { case x :: y :: z :: Nil => (x, y, z) }
  def forall(f: ((Int, Int, Int)) => Unit) { tup.foreach(f) }

  def map3D = this.mapD(_ => tup)
}