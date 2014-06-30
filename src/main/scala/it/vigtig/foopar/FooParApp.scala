package it.vigtig.foopar

import it.vigtig.foopar.comm.pure.PureDistributor
import it.vigtig.foopar.comm.Distributor
import it.vigtig.foopar.comm.pure.PureGroup
import it.vigtig.foopar.comm.CommGroup
import it.vigtig.foopar.comm.pure.PureGroup
import it.vigtig.foopar.comm.pure.FooParGroup

trait FooParApp {

  //Private members
  private var _args = Array[String]()

  protected[foopar] var _distributor: Option[Distributor] = None
  implicit val fpapp = this

  //Public members
  def globalRank: Int = _distributor.get.globalRank
  lazy val RANK: Int = _distributor.get.globalRank
  //  def hostName = java.net.InetAddress.getLocalHost().getHostName()
  lazy val worldSize = _distributor.get.worldSize
  def args = _args.clone
  lazy val globalGroup: FooParGroup = PureGroup((0 until worldSize).toArray, getNextGroup)
  private var _nextGroup = 0
  private def getNextGroup = { _nextGroup += 1; _nextGroup }
  private var next = 0
  def getGroup(size: Int) = {
    if (size > worldSize)
      sys.error("Not enough processes for group of size " + size)
    if (next + size >= worldSize) {
      next = size
      new PureGroup(0 until size toArray, getNextGroup)
    } else {
      val start = next
      next = next + size
      //      println("GROUP: " +start,next+"    "+((start until next) toArray).mkString(","))
      new PureGroup((start until next) toArray, getNextGroup)
    }
  }

  protected[foopar] def init(args: Array[String]) = {
    _args = args
  }

  implicit class IndexedSeqToDistSeq[T](seq: Array[T]) {
    def toDistSeq = it.vigtig.foopar.collection.DistSeq.fromSeq(seq)
  }
  implicit class SeqToDistSeq[T](seq: Seq[T]) {
    def toDistSeq = it.vigtig.foopar.collection.DistSeq.fromSeq(seq)
  }

  def pprint(s: Any*): Unit = print(s"Rank${_distributor.get.globalRank}: " + s.mkString(", "))
  def pprintln(s: Any) = pprint(s + "\n")
  def run()


}