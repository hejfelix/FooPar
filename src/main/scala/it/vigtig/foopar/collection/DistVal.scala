package it.vigtig.foopar.collection

import it.vigtig.foopar.FooParApp
import it.vigtig.foopar.comm.CommGroup
import it.vigtig.foopar.comm.pure.FooParGroup
import scala.reflect.ClassTag
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Builder
import scala.collection.mutable.ArrayBuffer

object DistVal {
  type DTB[T] = DistTraversableBuilder[T]
  type CBF[From, Elem, To] = CanBuildFrom[From, Elem, To]
  def apply[T](elem: T)(implicit fpapp: FooParApp) = new DistVal(Some(elem), fpapp.globalGroup)

  implicit def canBuildFrom[T, U](implicit fpapp: FooParApp): CBF[DistVal[T], U, DistVal[U]] =
    new CBF[DistVal[T], U, DistVal[U]] {
      def apply(): Builder[U, DistVal[U]] = newBuilder(None)
      def apply(from: DistVal[T]): Builder[U, DistVal[U]] =
        newBuilder(Some(from.group))
    }
  def newBuilder[T](group: Option[FooParGroup])(implicit fpapp: FooParApp): Builder[T, DistVal[T]] =
    group match {
      case Some(g) => new DTB[T].mapResult(t => new DistVal(arrayToHead(t), g))
      case None => new DTB[T] mapResult (t => DistVal(t(0)))
    }
  def arrayToHead[T](a: ArrayBuffer[T]) = if (a.isEmpty) None else Some(a.head)

}

class DistVal[+T](val elem: Option[T], val group: FooParGroup)(implicit val fpapp: FooParApp)
  extends DistTraversableLike[T, DistVal[T]]
  with TraversableNumOps[T, DistVal[T]] {
  def size = fpapp.worldSize
}
