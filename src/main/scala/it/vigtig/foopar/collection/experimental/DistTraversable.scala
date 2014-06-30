package it.vigtig.foopar.collection.experimental

import it.vigtig.foopar.comm.pure.FooParGroup

trait DistTraversable[+T] {

  def part: Seq[T]
  def foreach(f: T => Unit): Unit
  def group: FooParGroup

}