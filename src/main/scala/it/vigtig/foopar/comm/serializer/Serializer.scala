package it.vigtig.foopar.comm.serializer

import java.io.ByteArrayOutputStream
import java.io.ObjectInputStream
import java.io.ByteArrayInputStream
import java.io.ObjectOutputStream

class Serializer {

  def serialize[T](x: Any): Array[Byte] = {
    val baos = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(baos)
    oos.writeObject(x)
    oos.close()
    return baos.toByteArray()
  }

  def deSerialize[T](bytes: Any) = {
    val bais = new ByteArrayInputStream(bytes.asInstanceOf[Array[Byte]])
    val input = new ObjectInputStream(bais)
    val obj = input.readObject()
    input.close
    obj.asInstanceOf[T]
  }

}