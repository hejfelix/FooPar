package it.vigtig.foopar.comm

import it.vigtig.foopar.FooParApp

trait CommGroup {
  val fpapp: FooParApp
  def ranks: Array[Int]
  protected[comm] def getLocalProcess(i: Int): Process
  protected[comm] def processes: Array[Process]
  protected[comm] def localProcess: Process
  protected[comm] def localRank:Int
  def partOfGroup:Boolean
  def groupIndex: Int
  def size: Int
}