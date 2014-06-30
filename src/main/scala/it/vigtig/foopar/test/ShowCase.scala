package it.vigtig.foopar.test

import it.vigtig.foopar.FooParApp
import it.vigtig.foopar.FooParMain
import it.vigtig.foopar.collection.DistHashMap
import scala.util.Random
import it.vigtig.foopar.collection.DistVal
import it.vigtig.foopar.collection.DistSeq

object ShowCase extends FooParMain {

  def newApp(args: Array[String]) = new FooParApp {
	  
    /*
     * Comments list the output for p=4
     */
    def run {

      val dseq: DistSeq[Int] = (0 until worldSize).toDistSeq

      dseq.mapD(_ * 2).sumD.foreach(pprintln) //Rank0: 12
      dseq.avgD.foreach(pprintln) //Rank0: 1.5
      dseq.maxD.foreach(pprintln) //Rank0: 3
      dseq.minD.foreach(pprintln) //Rank0: 0

      var dhm = DistHashMap[Int, String]()

      //Get a shared random number generator
      val seedGen = new Random(System.currentTimeMillis())
      val distRnd: Option[Random] = DistVal(seedGen.nextInt)
        .allAvgD
        .map(_.toLong)
        .map(new Random(_))
        

      for {
        i <- 0 until 20
        r <- distRnd.map(_.nextInt(100))
      } dhm += r -> (if (r % 2 == 0) "even" else "odd")

      dhm.foreach(pprintln)
      /*
	Rank2: Map(42 -> even, 78 -> even, 38 -> even, 70 -> even, 34 -> even, 22 -> even, 54 -> even, 58 -> even, 30 -> even)
	Rank1: Map(1 -> odd, 33 -> odd, 97 -> odd, 41 -> odd)
	Rank3: Map(59 -> odd, 27 -> odd, 7 -> odd, 91 -> odd, 95 -> odd, 31 -> odd, 43 -> odd, 55 -> odd, 75 -> odd, 51 -> odd, 47 -> odd, 83 -> odd)
	Rank0: Map(0 -> even, 88 -> even, 56 -> even, 24 -> even, 60 -> even, 92 -> even, 96 -> even, 32 -> even, 12 -> even, 76 -> even, 80 -> even, 72 -> even, 68 -> even)
       */

    }

  }

}