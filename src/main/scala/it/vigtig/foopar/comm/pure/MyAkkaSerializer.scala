package it.vigtig.foopar.comm.pure

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import java.nio.ByteBuffer

import akka.serialization.Serializer
import it.vigtig.foopar.comm.serializer.CanSerialize
import it.vigtig.foopar.comm.serializer.FooParSerializer
import it.vigtig.foopar.examples.LazyMatrix

object MyAkkaSerializer {
  def main(args: Array[String]) {

    val x = GroupMsg(42, Array[Byte](1, 2, 3, 4))
    val mas = new MyAkkaSerializer
    val bin = mas.toBinary(x)
    println(bin.size)
//    val again = mas.fromBinary(bin, None).asInstanceOf[GroupMsg[_]]
//    println(again.myRank, again.msg.mkString(","))

  }
}

class MyAkkaSerializer extends Serializer {
  // This is whether "fromBinary" requires a "clazz" or not
  def includeManifest: Boolean = false

  def toByteArray(mat: LazyMatrix): Array[Byte] = {
    val bytes = Array.ofDim[Byte](16 + 8 * mat.data.size);
    var i = 0
    ByteBuffer.wrap(bytes).putDouble(mat.M).putDouble(mat.N)
    while (i < mat.data.size) {
      ByteBuffer.wrap(bytes).putDouble(mat.data(i))
      i = i + 1
    }
    return bytes;
  }

  def toDouble(bytes: Array[Byte]): Double = {
    return ByteBuffer.wrap(bytes).getDouble();
  }

  // Pick a unique identifier for your Serializer,
  // you've got a couple of billions to choose from,
  // 0 - 16 is reserved by Akka itself
  def identifier = 1337

  def toBytes(obj: AnyRef): Array[Byte] = {
    val baos = new ByteArrayOutputStream();
    val os = new ObjectOutputStream(baos);
    os.writeObject(obj);
    return baos.toByteArray()
  }

  val genericSerializer = new FooParSerializer[AnyRef] {
    def serialize(t: AnyRef): Array[Byte] = toBytes(t)
    def unserialize(a: Array[Byte]): AnyRef = {
      new ObjectInputStream(new ByteArrayInputStream(a)).readObject()
    }
  }

  def toBinary(obj: AnyRef): Array[Byte] = {
//      println("DOING SERIALIZATION!!! DUDEEEEE")
    if (obj.isInstanceOf[Array[Byte]]) {
//      println("Doing trivial serialization "+obj.asInstanceOf[Array[Byte]].size)
      return obj.asInstanceOf[Array[Byte]]
    }

    //    if (obj.isInstanceOf[GroupMsg]) {
    //      val gm = obj.asInstanceOf[GroupMsg]
    //      val result = Array.ofDim[Byte](4 + gm.msg.length)
    //      val rankBytes = ByteBuffer.allocate(4).putInt(gm.myRank).array()
    //      System.arraycopy(rankBytes, 0, result, 0, 4)
    //      System.arraycopy(gm.msg, 0, result, 4, gm.msg.length)
    //      return result
    //    }
    //    if (obj.isInstanceOf[GroupMsg[_ <: AnyRef]]) {
    //      val gm = obj.asInstanceOf[GroupMsg[_ <: AnyRef]]
    //
    //      val serializer = if (gm.msg.isInstanceOf[CanSerialize])
    //        gm.msg.asInstanceOf[CanSerialize].newSerializer
    //      else
    //        genericSerializer
    //
    //      val serializerBytes: Array[Byte] = toBytes(serializer)
    //
    //      val msgBytes = serializer.serialize(gm.msg)
    //
    //      var i = 0
    //      //      ByteBuffer.wrap(bytes).putDouble(gm.id)
    //      //      while (i < gm.msg.length) {
    //      //        ByteBuffer.wrap(bytes).put(gm.msg(i))
    //      //        i = i + 1
    //      //      }
    //      return bytes
    //    }
    sys.error("Couldn't serialize " + obj.getClass())
    ???
  }

  def fromBinary(bytes: Array[Byte],
    clazz: Option[Class[_]]): AnyRef = {
//    println("Doing trivial unserialization "+bytes.size)
    return bytes
    //    val rank = ByteBuffer.wrap(bytes).asIntBuffer().get()
    //    return GroupMsg(rank, bytes.drop(4))
    //    //    val msg = is.readObject().asInstanceOf[Array[Byte]]
    //    //    val bytes = Array.ofDim[Byte](4 + gm.msg.length);
    //    //    val in = new ObjectInputStream(bytes);
    //    // Put your code that deserializes here
    //    // ... ...
    //    //    sys.error("Couldn't deSerialize " + obj.getClass())
    //    ???
  }

}