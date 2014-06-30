package it.vigtig.foopar.test

import it.vigtig.foopar.FooParApp
import it.vigtig.foopar.FooParMain
import it.vigtig.foopar.collection.experimental.DistSeq

import scala.util.Random
import it.vigtig.foopar.collection.DistHashMap
import it.vigtig.foopar.collection.DistVal

//Statics are strictly forbidden
class PiApprox extends FooParApp {
  
  lazy val PRECISION = 1000000*worldSize

  def time(f: => Unit) = {
    val start = System.nanoTime
    f
    (System.nanoTime - start) / 1000000000d
  }

  def run() = {

    val Tp = time {
      pi(PRECISION) foreach (p => pprintln("Pi is approx. " + p + " compared to " + math.Pi))
    }

    pprintln(Tp)

    def fib(n: Int): Int = if (n < 2) n else fib(n - 1) + fib(n - 2)

  }

  def piBigDecimal(N: Int) = {
    val f = (x: BigDecimal) => 4d / (1d + x * x)
    val ff = (x: BigDecimal) => (x - 0.5d) / N
    DistSeq.ranged(1 to N)
      .mapD(BigDecimal(_))
      .mapD(f compose ff)
      .avgD
  }
  def pi(N: Int) = {
    val f = (x: Double) => 4d / (1d + x * x)
    val ff = (x: Int) => (x - 0.5d) / N

    DistSeq.ranged(1 to N)
      .mapD(f compose ff)
      .avgD
  }

  def piSequential(N: Int) = {
    val f = (x: Double) => 4d / (1d + x * x)
    val ff = (x: Int) => (x - 0.5d) / N

    (1 to N)
      .map(f compose ff)
      .sum / N
  }

}

object PiApprox extends FooParMain {
  def newApp(args: Array[String]) = new PiApprox
}

