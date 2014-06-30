package it.vigtig.foopar.test

import it.vigtig.foopar.FooParApp
import it.vigtig.foopar.FooParMain
import it.vigtig.foopar.collection.DistVal

class HelloWorld extends FooParApp {
  def run() {
    val hw = DistVal(globalRank)
    hw.sumD foreach pprintln
    hw(2) foreach pprintln
  }
}

object HelloWorld extends FooParMain {
  def newApp(args: Array[String]) = new HelloWorld
}