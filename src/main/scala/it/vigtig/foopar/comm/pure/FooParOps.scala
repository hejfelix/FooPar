package it.vigtig.foopar.comm.pure

import it.vigtig.foopar.comm.CommGroup
import it.vigtig.foopar.comm.serializer.Serializer
import scala.reflect.ClassTag
import scala.collection.mutable.ArrayBuffer
import java.nio.ByteBuffer
import it.vigtig.foopar.comm.serializer.CanSerialize
import java.io.ByteArrayOutputStream
import java.io.ObjectInputStream
import it.vigtig.foopar.comm.serializer.FooParSerializer
import java.io.ObjectOutputStream
import java.io.ByteArrayInputStream

trait FooParOps extends CommGroup {
  val serializer = new Serializer

  var opIndex = 0
  def nextOpIndex = { opIndex += 1; opIndex }

  def shift[T](elem: T, delta: Int): T = {
    implicit val srlz = getSerializerFor(elem)

    if (size == 1 || delta == size) {
      return elem
    }
    val OP_ID = Symbol(groupIndex + "shiftD" + nextOpIndex)
    val d = delta % size
    val time = System.nanoTime()

    send((localRank + d) % size, elem)
    val res: T = rcv((size + localRank - d) % size)
    return res
  }

  import math._
  def log2i(i: Int): Int = (log10(i) / log10(2)).toInt
  def log2d(i: Int): Double = (log10(i) / log10(2))
/*
  class GenericSerializer[T](val cl: java.lang.Class[T]) extends FooParSerializer[Any] {
    val kryo = new Kryo
    def serialize(t: Any): Array[Byte] = {
      val baos = new ByteArrayOutputStream();
      val os = new Output(baos);
      kryo.writeObject(os, t)
      return baos.toByteArray()
    }
    def unserialize(a: Array[Byte]): Any = {
      kryo.readObject(new Input(new ByteArrayInputStream(a)), cl)
    }
  }*/
  val genericSerializer: FooParSerializer[Any] =
    new FooParSerializer[Any] {
      def serialize(t: Any): Array[Byte] = {
        val baos = new ByteArrayOutputStream();
        val os = new ObjectOutputStream(baos);
        os.writeObject(t);
        return baos.toByteArray()
      }
      def unserialize(a: Array[Byte]): Any = {
        new ObjectInputStream(new ByteArrayInputStream(a)).readObject()
      }
    }

  var stime = 0l
  def send[T, U <: Any](sendIndex: Int, e: T, rnk: Int = localRank) = {
    var start = System.nanoTime()
    val srlz = genericSerializer
    val msgBytes = srlz.serialize(GroupMsg(ranks(rnk), e.asInstanceOf[U]))
    stime = stime + (System.nanoTime - start)
    getLocalProcess(sendIndex).!(msgBytes, fpapp.globalRank)
  }
  def rcv[T, U <: Any](fromRank: Int): T = {
    val srlz = genericSerializer
    //    fpapp.pprintln("Receiving from " + fromRank)
    val msg = getLocalProcess(fromRank).groupRcv[Array[Byte]](ranks(fromRank), -1)
    var start = System.nanoTime()
    val res = srlz.unserialize(msg).asInstanceOf[GroupMsg[T]]
    stime = stime + (System.nanoTime - start)
    res.msg
  }

  import math._
  def log2Ceil(i: Int): Int = ceil(log10(i) / log10(2)).toInt
  def pot(n: Int) = pow(2, n).toInt

  def fpOneToAll[T](elem: T, root: Int = ranks.head): T = {
    //    implicit val srlz = getSerializerFor(elem)
    var e: T = elem //Accumulator
    //"Transform" 0 to root by giving rt's element
    if (!(localRank == 0 && localRank == root)) {
      if (localRank == root)
        send(0, e)
      if (localRank == 0)
        e = rcv(root)
    }

    val ms = (0 until log2Ceil(size) map pot reverse)
    ms foreach { m =>
      if (localRank % (m * 2) == 0) {
        if (localRank + m < size) {
          send(localRank + m, e)
        }
      } else if (localRank % m == 0) { //receiving
        e = rcv(localRank - m)
      }
    }
    e
  }

  def fpReduce[T](elem: T, f: (T, T) => T, root: Int = ranks.head): Option[T] = {
    var value: T = elem
    for (m <- 0 until log2Ceil(size) map pot) {
      if (localRank % (m * 2) == 0) {
        if (localRank + m < size)
          value = f(value, rcv(localRank + m)) //Side-effect
      } else if (localRank % m == 0) {
        send(localRank - m, value) //Side-effect
      }
    }
    if (fpapp.globalRank == root) Some(value) else None
  }

  def prefixSum[T](elem: T, combiner: (T, T) => T): T = {
    implicit val srlz = getSerializerFor(elem)
    var result = elem
    var msg = result
    for (i <- 0 until log2Ceil(size)) {
      val partner = localRank ^ pow(2, i).toInt
//      println("rank: "+fpapp.globalRank+", partner:"+ranks(partner)+", iteration: "+i+",    size: "+size+";   "+(0 until log2Ceil(size)))
      if (partner < size) {
        send(partner, msg)
        val number: T = rcv(partner)
        msg = if (partner > localRank) combiner(msg, number) else combiner(number, msg)
        if (partner < localRank) result = combiner(number, result)
      }
    }
    return result
  }

  def prefixSumSeq[T](part: Seq[T], combiner: (T, T) => T): Seq[T] = {
    implicit val srlz = getSerializerFor(part)
    var result = part.head
    var msg = part.reduce(combiner)
    for (i <- 0 until log2i(size)) {
      val partner = localRank ^ math.pow(2, i).toInt
      send(partner, msg)
      val number: T = rcv(partner)
      msg = if (partner > localRank) combiner(msg, number) else combiner(number, msg)
      if (partner < localRank) result = combiner(number, result)
    }
    return (part.tail).scanLeft(result)(combiner)
  }

  def allReduce[T](elem: T, combiner: (T, T) => T): T = {
    implicit val srlz = getSerializerFor(elem)
    var result = elem
    for (i <- 0 until log2i(size)) {
      val partner = localRank ^ math.pow(2, i).toInt
      send(partner, result)
      result =
        if (partner < localRank)
          combiner(rcv(partner), result)
        else
          combiner(result, rcv(partner))
    }
    return result
  }

  def allToAll[T](elem: T): ArrayBuffer[T] = {
    implicit val srlz = getSerializerFor(elem)

    val left = (size + (localRank - 1)) % size
    val right = (localRank + 1) % size
    var res = ArrayBuffer.fill(size)(elem)
    var msg = elem
    for (i <- 0 until size) {
      val rsymb = Symbol(groupIndex + "a2a" + nextOpIndex + "," + i)
      send(right, msg)
      msg = rcv(left)
      res((size - i + localRank - 1) % size) = msg
    }
    res
  }

  def getSerializerFor[T](elem: T) =
    if (elem.isInstanceOf[CanSerialize[T]])
      elem.asInstanceOf[CanSerialize[T]].newSerializer
    else
      genericSerializer

  def allOneReduceSeq[T](elem: T, combiner: (T, T) => T, root: Int = ranks.head): Seq[T] = {
    implicit val srlz = getSerializerFor(elem)
    var acc: T = elem //Accumulator
    var m = 1
    for (r <- 1 to ceil(log2d(size)).toInt) {
      val rsymb = Symbol(groupIndex + "aor" + nextOpIndex + " " + r)
      if (localRank % (m * 2) == 0) { //receiving
        if (localRank + m < size) {
          acc = combiner(acc, rcv(localRank + m)) //Accumulate
        }
      } else if (localRank % m == 0) { //sending
        send(localRank - m, acc)
      }
      m = m << 1
    }

    if (fpapp.globalRank == root)
      Seq(acc)
    else
      Nil
  }

  def zip[T, B](part: Seq[_], thatGroup: FooParGroup): Seq[(T, B)] = {
    implicit val srlz = getSerializerFor(part)
    if (thatGroup.partOfGroup) {
      send(thatGroup.localRank, part, thatGroup.localRank)
    }
    val msg: Seq[B] = if (partOfGroup) {
      rcv(localRank)
    } else Nil
    part.zip(msg).asInstanceOf[Seq[(T, B)]]
  }

  def barrier {
    def idLeft(a: Any, b: Any) = a
    val BARRIER = 'BARR
    fpOneToAll(Some(BARRIER), 0).get
    fpReduce(BARRIER, idLeft, ranks.head)
  }

}
