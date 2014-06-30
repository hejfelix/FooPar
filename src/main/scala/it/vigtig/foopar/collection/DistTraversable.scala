package it.vigtig.foopar.collection

import it.vigtig.foopar.comm.pure.FooParGroup

trait DistTraversable[+T] {

  def elem: Option[T]
  def foreach(f: T => Unit):Unit
  def group:FooParGroup

}