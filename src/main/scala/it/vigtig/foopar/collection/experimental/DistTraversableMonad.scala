package it.vigtig.foopar.collection.experimental

import scala.collection.generic.CanBuildFrom

object DistTraversableMonad {
  implicit class DistTraversableMonad[+T, +Repr](dt: DistTraversableLike[T, Repr]) {
    def filter[That](p: T => Boolean)(implicit bf: CanBuildFrom[Repr, T, That]): That = dt.filterD(p)
    def map[B, That](f: T => B)(implicit bf: CanBuildFrom[Repr, B, That]): That = dt.mapD(f)
    def flatMap[B, That](f: T => Traversable[B])(implicit bf: CanBuildFrom[Repr, B, That]): That = dt.flatMapD(f)
  }
}