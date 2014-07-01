package it.vigtig.foopar.examples

import it.vigtig.foopar.collection.DistTraversableLike
import it.vigtig.foopar.comm.pure.FooParGroup
import it.vigtig.foopar.collection.DistGrid
import it.vigtig.foopar.FooParMain
import it.vigtig.foopar.FooParApp
import it.vigtig.foopar.collection.DistGrid3D

object GridTest extends FooParMain {

  def newApp(args: Array[String]) = new FooParApp {
    def run {

      def base4toInt(quad: Seq[Int]) = {
        val bases = Seq(4, 4, 4).scanRight(1)(_ * _).tail
        (quad, bases).zipped.map(_ * _).sum
      }

      val R = 0 to 3
      val g = DistGrid(R, R, R)
      val sumOfRanks = (0 to 63).sum
      g.mapD(base4toInt).sumD.map(_ == sumOfRanks).foreach(pprintln)

//      g.maxByD(base4toInt).foreach(pprintln)

      //      g.countD(_.count(_ == 0) >= 2).foreach(pprintln)
      //      g.existsD(_.length < 2).foreach(pprintln)
      //      g.existsD(_.length >= 2).foreach(pprintln)
      //      g.forallD(_.length == 3).foreach(pprintln)
      //      g.forallD(_.contains(1)).foreach(pprintln)
      //      g.forallD(_.sum >= 0).foreach(pprintln)
      //      g.mapD(_.sum).sumD.foreach(pprintln)
      //      g.xs.countD(_.sum > 2).foreach(pprintln)
      //      g.ys.countD(_.sum > 2).foreach(pprintln)
      //      g.zs.countD(_.sum > 2).foreach(pprintln)
      //      g.zs.reduceD(_ ++ _).foreach(pprintln)

      //      Thread.sleep(RANK*200)
      //      g foreach pprintln
      //      Thread.sleep(20000)
      //      val seq = 0 until worldSize
      //      val dseq = seq.toDistSeq
      //
      //      //      pprintln(
      //      //        dseq.filterD(_ % 2 == 0).mapD(_ + 1).allSumD,
      //      //        seq.filter(_ % 2 == 0).map(_ + 1).sum)
      //
      //      val A = Array(
      //        Array(1, 2, 3),
      //        Array(4, 5, 6),
      //        Array(7, 8, 9))
      //      val B = Array(
      //        Array(1, 0, 0),
      //        Array(0, 1, 0),
      //        Array(0, 0, 1))
      //      //
      //      val dim = 0 until A.size
      //      val g = DistGrid(dim, dim, dim)
      //      //
      //      //      val g2 = DistGrid(0 to 1, 2 to 3, 4 to 5) mapD { case x :: y :: z :: Nil => List(x + 4, y, z * z) }
      //      //
      //      //      val res = g.mapD { case (i, j, k) => A(i)(k) * B(k)(j) }.xs2.sumD 
      ////      Thread.sleep(RANK * 200)
      //      //      g foreach pprintln
      //      g.zs.apply(0) foreach pprintln

      //      g.mapD(_.sum).xs.sumD foreach pprintln
      //      g.mapD(_.sum).ys.sumD foreach pprintln

    }
  }

}