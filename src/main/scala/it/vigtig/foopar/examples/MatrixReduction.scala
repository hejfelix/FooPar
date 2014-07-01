package it.vigtig.foopar.examples

import it.vigtig.foopar.FooParApp
import it.vigtig.foopar.FooParMain
import scala.util.Random
import it.vigtig.foopar.config.ArgsParser
import it.vigtig.foopar.collection.DistSeq
import it.vigtig.foopar.config.HostManager
import it.vigtig.foopar.comm.serializer.CanSerialize
import it.vigtig.foopar.comm.serializer.FooParSerializer
import java.nio.ByteBuffer

object LazyMatrix {
  def apply(M: Int, N: Int, SEED: Int) = new LazyMatrix(SEED, M, N)
  def apply(M: Int, N: Int, d: Array[Double]) = new LazyMatrix(-1, M, N, Some(d))

  def main(args: Array[String]) {
    val a = LazyMatrix(5, 5, 1337)
    val bytes = a.newSerializer.serialize(a)
    val aa = a.newSerializer.unserialize(bytes)
    println(a, aa)
  }

}

case class LazyMatrix(val SEED: Int, M: Int, N: Int, d: Option[Array[Double]] = None)
  extends CanSerialize[LazyMatrix] {

  def newSerializer = new FooParSerializer[LazyMatrix] {
    def serialize(t: LazyMatrix): Array[Byte] = {
      val time = System.nanoTime()
      val bytes = Array.ofDim[Byte](8 + t.data.size * 8)
      val buf = ByteBuffer.wrap(bytes)
      buf.putInt(t.M)
      buf.putInt(t.N)
      var i = 0
      while (i < t.data.size) {
        buf.putDouble(t.data(i))
        i = i + 1
      }
      //      println("Time taken: "+(System.nanoTime-time)/math.pow(10,9))
      bytes
    }
    def unserialize(bytes: Array[Byte]): LazyMatrix = {
      val buf = ByteBuffer.wrap(bytes)
      val M = buf.getInt()
      val N = buf.getInt()
      var i = 0
      val data = Array.ofDim[Double](M * N)
      while (i < M * N) {
        data(i) = buf.getDouble()
        i = i + 1
      }
      new LazyMatrix(-1, M, N, Some(data))
    }
  }

  lazy val data = {
    d.getOrElse {
      val rnd = new Random(SEED)
      Array.fill(M * N)(rnd.nextInt(10000).toDouble)
    }
  }
  def +(that: LazyMatrix) = {
    val res = Array.ofDim[Double](M * N)
    var i = 0
    while (i < M * N) {
      res(i) = data(i) + that.data(i)
      i = i + 1
    }
    LazyMatrix(M, N, res)
  }

  def *(that: LazyMatrix) = {
    val res = Array.ofDim[Double](M * that.N)
    var i = 0
    it.vigtig.foopar.test.MatrixOps.mult(data, that.data, res, M, N, that.N)
    //    while (i < M) {
    //      var j = 0
    //      while (j < N) {
    //        var k = 0
    //        while (k < M) {
    //          res(i + M * j) = res(i + M * j) + data(i + M * k) * that.data(k + M * j)
    //          k = k + 1
    //        }
    //        j = j + 1
    //      }
    //      i = i + 1
    //    }
    LazyMatrix(M, that.N, res)
  }

  def *(d: Double) = {
    val res = Array.ofDim[Double](M * N)
    var i = 0
    while (i < M * N) {
      res(i) = data(i) * d
      i = i + 1
    }
    LazyMatrix(M, N, res)
  }
  def equals(that: LazyMatrix): Boolean = {
    for (i <- 0 until M * N) {
      if (data(i) != that.data(i))
        return false
    }
    return true
  }
  override def toString = data.mkString(",")
}

class MatrixReduction extends FooParApp with ArgsParser with HostManager {

  def run = {

    //    if (globalRank == 0) {
    //      new Thread {
    //        import sys.process.stringSeqToProcess
    //        override def run() {
    //          while (true) {
    //            print(Seq("bash", "-c", "top -b -n 1 | grep java").!!)
    //            Thread.sleep(1000)
    //          }
    //        }
    //      }.start()
    //    }

    val N = args.getValue("-N").toInt

    val glob = (0 until worldSize).toDistSeq

    if (globalRank == 0)
      println("Doing warmup")

    val warmupSize = 40
    val a = LazyMatrix(1337, warmupSize, warmupSize)
    val b = LazyMatrix(1338, warmupSize, warmupSize)
    for (i <- 0 until 100) {
      val c = a + b
      c.data
    }

    if (globalRank == 0) {
      println("Starting benchmark with block size " + N + "\n")
      println("n, rounds, avg. time")
    }
    for (i <- worldSize to worldSize) {
      val mats = Array.fill(i)(LazyMatrix(N, N, 1337))
      val dseq = mats.toDistSeq
      dseq.foreach(_.data) //Inflate locally

      //Try to do GC
      System.gc(); System.gc();

      glob.align
      bench(dseq)
    }
  }

  def bench(dseq: DistSeq[LazyMatrix]) {

    val N = args.getValue("-N").toInt
    val ROUNDS = args.getValue("-R").toInt
    val start = System.nanoTime()
    var result: Option[LazyMatrix] = None
    for (i <- 0 until ROUNDS) {
      result = dseq.reduceD(_ * _)
    }
    val time = System.nanoTime() - start
    val bench = (time / math.pow(10, 9)) / ROUNDS.toDouble

    result.foreach { x => //Only root will contain answer
      //      if (!x.equals(LazyMatrix(N, N, 1337) * dseq.size)) {
      //        sys.error("Wrong matrix result!")
      //      }
      println(dseq.size + "," + ROUNDS + "," + bench)
    }
  }

}
object MatrixReduction extends FooParMain {
  def newApp(args: Array[String]) = new MatrixReduction
}