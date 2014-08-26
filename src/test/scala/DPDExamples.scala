import it.vigtig.foopar.FooParApp
import it.vigtig.foopar.FooParMain
import scala.util.Random
import it.vigtig.foopar.collection.DistHashMap
import it.vigtig.foopar.collection.DistVal
import it.vigtig.foopar.collection.DistSeq
import it.vigtig.foopar.collection.DistGrid

class DPDExamples extends FooParApp {


  def time(f: => Unit) = {
    val start = System.nanoTime
    f
    (System.nanoTime - start) / 1000000000d
  }

  def run() = {
  

    /*
      For DistVal, we can use commutative binary funcs
    */
    val pTime = DistVal(System.currentTimeMillis)
    pTime.maxD foreach pprintln    
    pTime.minD foreach pprintln
      
    /*
      For DistSeq, we can use associative binary funcs
    */
    val dSeq = DistSeq.fill(worldSize)( i => ('a'+i).toChar )
    ( dSeq mapD (_.toString) ).reduceD(_++_) foreach pprintln    

    /*
      For DistGrid we can represent mixed radix
    */
    val B = 3
    val R = 0 until B
    val g = DistGrid(R, R, R) //R x R x R, >=27 processing elements

    def baseBToInt(quad: Seq[Int]) = {
      val bases = Seq(B, B, B).scanRight(1)(_ * _).tail
      (quad, bases).zipped.map(_ * _).sum
    }

    val sumOfRanks = (0 until g.size).sum
    g.mapD(baseBToInt).sumD.map(_ == sumOfRanks).foreach(pprintln)  
    g.maxByD(baseBToInt).foreach(x => pprintln(s"Max is $x") )     
    g.minByD(baseBToInt).foreach(x => pprintln(s"Min is $x") )  
    g.mapD(baseBToInt).avgD.foreach(x => pprintln(s"Avg. is $x") )
  }

}

object DPDExamples extends FooParMain {
  def newApp(args: Array[String]) = new DPDExamples
}

