import scala.math.log10
import scala.util.Random

import org.scalacheck.Gen
import org.scalacheck.Gen.choose
import org.scalacheck.Gen.listOf1
import org.scalacheck.Gen.resize
import org.scalacheck.Prop.BooleanOperators
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.propBoolean
import org.scalacheck.Properties
import org.scalacheck.Test
import org.scalacheck.Test.Parameters

import it.vigtig.foopar.FooParMain
import it.vigtig.foopar.collection.DistVal
import it.vigtig.foopar.collection.DistGrid
import it.vigtig.foopar.config.ArgsParser

object FooParChecks extends FooParMain {
  def newApp(args: Array[String]) = new DistSeqSuite
}

class DistSeqSuite extends FooParTestApp("DistSeqSuite") with ArgsParser {

  //    override def DEBUG_PRINT = true

  implicit class Should[T](opt: Option[T]) {
    def shouldEqual[U](x: U) = opt.map(_ == x).getOrElse(true)
  }

  override def run() = {
    if (globalRank == 0)
      println(s"""Starting test suite "${name}" with ${worldSize} processing elements""")

    val prm: Parameters = new Test.Parameters.Default {
      override val minSuccessfulTests: Int = 1000
      override val rng = new Random(1337)
    }

    val ints: Gen[Int] = choose(Int.MinValue, Int.MaxValue)
    val doubles: Gen[Double] = choose(Double.MinValue, Double.MaxValue)
    val naturals: Gen[Int] = choose(0, Int.MaxValue)
    implicit lazy val sizedLists = resize(worldSize, listOf1(ints))
    implicit lazy val sizedDoubleList = resize(worldSize, listOf1(doubles))

    property("prefix sum on ranks") = {
      val xs = 0 until worldSize
      val dseq = xs.toDistSeq
      val expected = (xs.tail scanLeft xs.head)(_ + _)
      val res = dseq.scan1D(_ + _)
      val rank = dseq.group.localRank
      rank >= 0 && (res shouldEqual expected(rank))
    }

    property("scan1D doesn't deadlock") = forAll(sizedLists) { xs: List[Int] =>
      if (xs.size < 1) {
        true
      } else {
        val expect = (xs.tail scanLeft xs.head)(_ + _)
        val dseq = xs.toDistSeq
        val res = dseq.scan1D(_ + _)
        val rank = dseq.group.localRank
        if (rank >= 0) {
          (res shouldEqual expect(rank))
        } else {
          true
        }
      }
    }

    property("align doesn't deadlock") = forAll(sizedLists) { xs: List[Int] =>
      xs.toDistSeq.align
      true
    }

    property("sumD should be xs.sum") = forAll(sizedLists) { xs: List[Int] =>
      val dseq = xs.toDistSeq
      dseq.sumD shouldEqual xs.sum
    }

    /*
     * There are precision problems when using floating point
     */
//    property("sumD double") = forAll(sizedDoubleList) { (xs: List[Double]) =>
//      val dseq = xs.toDistSeq
//      dseq.sumD shouldEqual xs.sum
//    }

    property("sumD should be sum") = forAll(sizedLists) { xs: List[Int] =>
      val dseq = xs.toDistSeq
      dseq.sumD shouldEqual xs.sum
    }

    property("avgD behaves like xs.sum/xs.size.toDouble") = forAll(sizedLists) { xs: List[Int] =>
      val dseq = xs.toDistSeq
      dseq.avgD shouldEqual (xs.sum / xs.size.toDouble)
    }
    property("dseq(n) should be xs(n)") = forAll(sizedLists, naturals) { (xs: List[Int], i: Int) =>
      val dseq = xs.toDistSeq
      dseq(i % xs.size) shouldEqual xs(i % xs.size)
    }

    property("maxD should be max") = forAll(sizedLists) { xs: List[Int] =>
      val dseq = xs.toDistSeq
      dseq.maxD shouldEqual xs.max
    }

    property("dseq.map(_.toString).reduceD(_+_) should be xs.mkString") = forAll(sizedLists) { xs: List[Int] =>
      val dseq = xs.toDistSeq.mapD(_.toString)
      dseq reduceD (_ + _) shouldEqual xs.mkString
    }

    property("minD should be min") = forAll(sizedLists) { (xs: List[Int]) =>
      val dseq = xs.toDistSeq
      dseq.minD shouldEqual xs.min
    }

    this.check(prm)
    (new DistValSuite).check(prm)
    (new DistGridSuite(worldSize)).check(prm)

  }

  class DistGridSuite(worldSize: Int) extends Properties("DistGridSuite") {

    import math._
    def log2i(n: Int) = (log10(n) / log10(2)).toInt
    val maxBinDim = log2i(worldSize)
    val binaryRanges = for (i <- 0 until maxBinDim) yield 0 to 1

    property("align doesn't deadlock") = {
      val dg = DistGrid(binaryRanges: _*)
      dg.align
      true
    }

  }

  class DistValSuite() extends Properties("DistValSuite") {

    //  override def DEBUG_PRINT = true

    property("align doesn't deadlock") = {
      val x = DistVal('Nothing)
      x.align
      true
    }

    property("oneToAll broadcast from n should yield n for all PEs") = forAll { n: Int =>
      (n >= 0) ==> {
        val x = DistVal(globalRank)
        x(n % worldSize) shouldEqual (n % worldSize)
      }
    }

    property("sumD should be sum of ranks at root") = forAll { n: Int =>
      val x = DistVal(globalRank)
      x.sumD shouldEqual (0 until worldSize sum)
    }

    property("productD should be product of ranks at root") = forAll { n: Int =>
      val x = DistVal(globalRank)
      x.productD shouldEqual (0 until worldSize product)
    }

    property("reduceD with concat yields string of ranks (size-changing input)") = forAll { str: String =>
      val rankString = DistVal(str)
      val expected = 0 until worldSize map (_ => str) mkString ""
      rankString reduceD (_ + _) shouldEqual expected
    }

    property("avgD should be same as sum/worldSize") = forAll { d: Int =>
      val x = DistVal(d)
      val res = x.avgD
      val expect = (0 until worldSize map (_ => d)).sum / worldSize.toDouble
      res shouldEqual expect
    }

    property("scan1D should be same as scan") = forAll { n: Int =>
      val x = DistVal(globalRank)
      val res = x.scan1D(_ + _)
      val ranks = 0 until worldSize
      val expect = (ranks.tail.scanLeft(ranks.head)(_ + _))
      res shouldEqual (expect(globalRank))
    }

  }

}

