package it.vigtig.foopar.collection

import scala.collection.mutable.Builder
import scala.collection.mutable.ArrayBuffer

class DistTraversableBuilder[T](var elem: Option[T] = None) extends ArrayBuffer[T](1) {
  override def +=(elem: T): this.type = {
    ensureSize(1)
    array(0) = elem.asInstanceOf[AnyRef]
    size0 = 1
    this
  }
  override def clear() { size0 = 0 }
}