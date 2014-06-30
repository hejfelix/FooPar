package it.vigtig.foopar.collection

import scala.collection.immutable.HashMap
import it.vigtig.foopar.FooParApp
import it.vigtig.foopar.comm.pure.FooParGroup
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Builder

object DistHashMap {
  def apply[K, V]()(implicit fpapp: FooParApp): DistHashMap[K, V] =
    new DistHashMap(new HashMap[K, V](), fpapp.globalGroup)
  def apply[K, V](hm: HashMap[K, V])(implicit fpapp: FooParApp): DistHashMap[K, V] =
    new DistHashMap(hm, fpapp.globalGroup)
  def apply[K, V](hm: HashMap[K, V], dhm: DistHashMap[K, V])(implicit fpapp: FooParApp): DistHashMap[K, V] =
    new DistHashMap(hm, dhm.group)

  //  implicit def canBuildFrom[K, T, U](implicit fpapp: FooParApp): CanBuildFrom[DistHashMap[K, T], HashMap[K, U], DistHashMap[K, U]] =
  //    new CanBuildFrom[DistHashMap[K, T], HashMap[K, U], DistHashMap[K, U]] {
  //      def apply(): Builder[HashMap[K, U], DistHashMap[K, U]] = newBuilder(None, 0)
  //      def apply(from: DistHashMap[K, T]): Builder[HashMap[K, U], DistHashMap[K, U]] = newBuilder(Some(from.group), from.size)
  //    }
  //  def newBuilder[K, T](group: Option[FooParGroup], size: Int)(implicit fpapp: FooParApp): Builder[HashMap[K, T], DistHashMap[K, T]] =
  //    group match {
  //      case Some(g) => new DistTraversableBuilder[HashMap[K, T]] mapResult (t => new DistHashMap[K, T](t.head, g))
  //      case None => new DistTraversableBuilder[HashMap[K, T]] mapResult (t => DistHashMap[K, T](t.head))
  //    }
}
class DistHashMap[K, V](private val hmap: HashMap[K, V], val group: FooParGroup) extends DistTraversableLike[HashMap[K, V], DistHashMap[K, V]] {

  def elem = Some(hmap)
  def size = hmap.size

  def +(elem: (K, V))(implicit fpapp: FooParApp) = {
    if (elem._1.hashCode % fpapp.worldSize == fpapp.globalRank)
      DistHashMap(hmap + elem, this)
    else
      this
  }

  def get(K: K)(implicit fpapp: FooParApp) = {
    val hashRank = K.hashCode % fpapp.worldSize
    val e = hmap.getOrElse(K, null.asInstanceOf[V])
    group.fpOneToAll(Some(e), hashRank)
  }

}