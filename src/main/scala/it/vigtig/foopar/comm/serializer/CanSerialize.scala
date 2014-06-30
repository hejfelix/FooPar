package it.vigtig.foopar.comm.serializer

trait CanSerialize[T] {

  def newSerializer: FooParSerializer[T]

}