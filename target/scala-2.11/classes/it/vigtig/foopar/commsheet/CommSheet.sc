package it.vigtig.foopar.commsheet

object CommSheet {

  abstract class Mutation {
  	def mutate[T](x:T):T = x
  }
  case class Send(destination: Int) extends Mutation
  case class Receive(source: Int) extends Mutation
  
  case object Nop extends Mutation

  import math._

  val n = 10                                      //> n  : Int = 10

  def log2(i: Int): Int = (log10(i) / log10(2)).toInt
                                                  //> log2: (i: Int)Int


  def reduce(p: Int)(rank: Int) = {
    def mutation(r: Int) = {
      val m = math.pow(2, r).toInt
      if (rank % (m * 2) == 0 && rank + m < p)
        Receive(rank + m)
      else if (rank % m == 0)
        Send(rank - m)
      else
        Nop
    }
    0 to log2(p) map mutation
  }                                               //> reduce: (p: Int)(rank: Int)scala.collection.immutable.IndexedSeq[Product wit
                                                  //| h Serializable with it.vigtig.foopar.commsheet.CommSheet.Mutation]

  val mutators = 0 until n map reduce(n)          //> mutators  : scala.collection.immutable.IndexedSeq[scala.collection.immutable
                                                  //| .IndexedSeq[Product with Serializable with it.vigtig.foopar.commsheet.CommSh
                                                  //| eet.Mutation]] = Vector(Vector(Receive(1), Receive(2), Receive(4), Receive(8
                                                  //| )), Vector(Send(0), Nop, Nop, Nop), Vector(Receive(3), Send(0), Nop, Nop), V
                                                  //| ector(Send(2), Nop, Nop, Nop), Vector(Receive(5), Receive(6), Send(0), Nop),
                                                  //|  Vector(Send(4), Nop, Nop, Nop), Vector(Receive(7), Send(4), Nop, Nop), Vect
                                                  //| or(Send(6), Nop, Nop, Nop), Vector(Receive(9), Send(6), Send(4), Send(0)), V
                                                  //| ector(Send(8), Nop, Nop, Nop))
  


}