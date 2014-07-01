package it.vigtig.foopar.test

import it.vigtig.foopar.FooParApp
import it.vigtig.foopar.FooParMain
import it.vigtig.foopar.collection.DistVal

class HelloWorld extends FooParApp {

  /*
   * Concatenates "Hello, World! " with itself 
   * for as many characters as there are processing elements
   */

  val hello = Stream.continually("Hello, World! ".toStream).flatten

  def run() {
    
    pprintln("I'm alive :-D")
    
    val hw = DistVal((hello drop globalRank).head) mapD (_.toString)
    hw reduceD (_ ++ _) foreach pprintln
  }
}

object HelloWorld extends FooParMain {
  def newApp(args: Array[String]) = new HelloWorld
}