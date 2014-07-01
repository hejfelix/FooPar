package it.vigtig.foopar.examples

import it.vigtig.foopar.FooParApp
import it.vigtig.foopar.FooParMain
import it.vigtig.foopar.collection.DistGrid
import scala.util.Random
import it.vigtig.foopar.collection.DistVal
import it.vigtig.foopar.config.ArgsParser
import it.vigtig.foopar.comm.pure.FooParAkkaProcess

object Floyd extends FooParMain {

  case class LazyMatrix(N: Int, M: Int, SEED: Int) {
    lazy val data = {
      val rnd = new Random(SEED)
      Array.fill(M, N)(rnd.nextInt(10).toFloat)
    }
  }

  def blockify[T](a: Array[Array[LazyMatrix]], n: Int) =
    a.map(_.grouped(n).toArray).grouped(n).toArray.map(_.transpose)
  def blockify(a: Array[Array[Float]], n: Int) =
    a.map(_.grouped(n).toArray).grouped(n).toArray.map(_.transpose)

  def string(data: Array[Array[Float]]) = data.map(_.mkString(",")).mkString("\n")
  val rndg = new Random(1337)
  val INSTANCE_RND = Array.fill[Float](8, 8)(rndg.nextInt(10).toFloat)
  /* solution
		1.0,2.0,3.0,2.0,4.0,2.0,2.0,2.0
		0.0,0.0,1.0,2.0,3.0,0.0,2.0,0.0
		0.0,2.0,3.0,2.0,3.0,2.0,2.0,2.0
		0.0,0.0,1.0,2.0,2.0,0.0,0.0,0.0
		0.0,2.0,1.0,2.0,0.0,0.0,2.0,2.0
		0.0,2.0,1.0,2.0,4.0,2.0,2.0,2.0
		0.0,0.0,1.0,2.0,2.0,0.0,2.0,0.0
		0.0,0.0,1.0,2.0,3.0,0.0,2.0,0.0
   */

  val INF = Float.MaxValue

  val INSTANCE_A = Array(
    Array(0f, 2f, 4f, 12f, 15f),
    Array(2f, 0f, 1f, INF, INF),
    Array(4f, 1f, 0f, 1f, INF),
    Array(12f, INF, 1f, 0f, 2f),
    Array(15f, INF, INF, 2f, 0))

  /*
     * SOL:
		0.0,2.0,3.0,4.0,6.0
		2.0,0.0,1.0,2.0,4.0
		3.0,1.0,0.0,1.0,3.0
		4.0,2.0,1.0,0.0,2.0
		6.0,4.0,3.0,2.0,0.0
     */

  def newApp(args: Array[String]) = new FooParApp with ArgsParser {

    def benchmark(b: => Unit)(implicit rounds: Int = 5) {
      var times = List[Long]()
      for (_ <- 1 to rounds) {
        val (t, res) = time(b)
        times = t +: times
      }
      val SECONDS = 1000000000.toDouble
      val AVG = (times.sum / rounds.toDouble) / SECONDS
      val MAX = times.max / SECONDS
      val MIN = times.min / SECONDS
      if (RANK == 0)
        pprintln("Average time: " + AVG + ", max time: " + MAX + ", min time: " + MIN + "\nfrom " + rounds + " runs")
    }
    def time[T](b: => T): (Long, T) = {
      val start = System.nanoTime()
      val res = b
      ((System.nanoTime() - start), res)
    }

    implicit var rounds = 5
    def run() {
      rounds = args.getValueOr("-R", "5").toInt
      val N = args.getValue("-N").toInt
      val SERIAL = args.getValueOr("-SEQUENTIAL", "false")

      //      pprintln("HELLOOO")

      if (N % math.sqrt(worldSize) != 0)
        sys.error("N must be divisible by sqrt(worldSize)! Shutting down...")

      if (RANK == 0)
        pprintln("Starting benchmark with N=" + N + ", rounds=" + rounds + " and p=" + worldSize)
      if (RANK == 0 && SERIAL == "true")
        floydSerial(N)
      floydBench(N)
    }

    def floydSerial(N: Int) {
      val rndg = new Random(1337)
      val instance = Array.fill[Float](N, N)(rndg.nextInt(10).toFloat)
      val M = instance.length
      val A = instance
      benchmark {
        for (k <- 0 until M; i <- 0 until M; j <- 0 until M) {
          A(i)(j) = math.min(A(i)(j), A(i)(k) + A(k)(j))
        }
      }
    }

    def floydBench(N: Int) {

      val dim = math.sqrt(worldSize).toInt
      val BS = N / dim //Block size
      val R = 0 until dim
      val BLOCK = 0 until BS
      var grid = DistGrid(R, R) mapD { case i :: j :: Nil => LazyMatrix(BS, BS, RANK).data }

      val xgrp = grid.xs.group
      val ygrp = grid.ys.group

      var commTime = 0l
      benchmark {
        for (k <- 0 until N) {

          val _ik = grid.ys.mapD(_(k % BS))
          val _kj = grid.xs.mapD(_.map(_(k % BS)))

          val (t1, ik) = time { _ik.apply(k / BS).get }
          val (t2, kj) = time { _kj.apply(k / BS).get }
          commTime = commTime + t1 + t2

          grid = grid.mapD { block =>
            for (i <- BLOCK; j <- BLOCK) {
              block(i)(j) = math.min(block(i)(j), ik(j) + kj(i))
            }
            block
          }
        }
      }

      pprintln("Time spent on comm: " + (commTime / 1000000000d) / rounds.toDouble)

    }

    def floydPar(N: Int) {

      val dim = math.sqrt(worldSize).toInt
      val BS = N / dim //Block size
      val R = 0 until dim
      val BLOCK = 0 until BS
      var grid = DistGrid(R, R) mapD { case i :: j :: Nil => LazyMatrix(BS, BS, RANK).data }

      val xgrp = grid.xs.group
      val ygrp = grid.ys.group

      for (k <- 0 until N) {

        val ik = grid.ys.mapD(_(k % BS)).apply(k / BS).get
        val kj = grid.xs.mapD(_.map(_(k % BS))).apply(k / BS).get

        grid = grid.mapD { block =>
          for (i <- BLOCK; j <- BLOCK) {
            block(i)(j) = math.min(block(i)(j), ik(j) + kj(i))
          }
          block
        }
      }

    }

    case class LazyBlock(data: Array[Array[Double]])
    type LazyMatrix = Array[Array[LazyBlock]]
    type Matrix = Array[Array[Double]]
    type Vector = Array[Double]
    def update(row: Vector, col: Vector)(mat: Matrix) = {
      for (i <- 0 until mat.size; j <- 0 until mat(0).size)
        mat(i)(j) = math.min(mat(i)(j), row(j) + col(i))
      mat
    }
    def floydPar(blocks: LazyMatrix, BS: Int) = {
      val dim = blocks.size
      val R = 0 until dim
      val N = dim * BS
      var grid = DistGrid(R, R) mapD { case i :: j :: Nil => blocks(i)(j).data }

      for (k <- 0 until N) {
        val ik = grid.ys.mapD(_(k % BS)).apply(k / BS).get
        val kj = grid.xs.mapD(_.map(_(k % BS))).apply(k / BS).get
        grid = grid.mapD(update(ik, kj))
      }
      grid
    }
  }

}