package it.vigtig.foopar.examples

object Hypercube {

  import math._
  def log2i(i: Int): Int = (log10(i) / log10(2)).toInt
  def main(args: Array[String]): Unit = {

    val n = 8
    val rank = 4
    for (i <- 0 until log2i(n)) {
      val partner = rank ^ pow(2, i).toInt
      println(partner)
    }

  }

}