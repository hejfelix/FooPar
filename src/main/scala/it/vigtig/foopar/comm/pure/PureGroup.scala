package it.vigtig.foopar.comm.pure

import it.vigtig.foopar.FooParApp
import it.vigtig.foopar.comm.CommGroup
import it.vigtig.foopar.comm.serializer.Serializer

case class PureGroup(override val ranks: Array[Int], val groupIndex: Int)(implicit _fpapp: FooParApp) extends FooParOps {
  //  println(" GROUP IS "+ranks.mkString(","))
  val fpapp: FooParApp = _fpapp
  protected[comm] val processes = fpapp._distributor.get.processes
  protected[comm] def getLocalProcess(i: Int) = processes(ranks(i))
  lazy val partOfGroup = ranks.contains(_fpapp.globalRank)
  lazy val localProcess = processes(fpapp.globalRank)
  lazy val localRank = ranks.indexOf(_fpapp.globalRank)
  lazy val size = ranks.size
  lazy val isLastRank = localRank == ranks.last

  def subGroup(rnks: Array[Int]) =
    PureGroup(rnks, groupIndex)(_fpapp)
}