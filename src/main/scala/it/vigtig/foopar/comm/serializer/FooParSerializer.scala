package it.vigtig.foopar.comm.serializer

trait FooParSerializer[T] {

  def serialize(t: T): Array[Byte]
  def unserialize(a: Array[Byte]): T

}