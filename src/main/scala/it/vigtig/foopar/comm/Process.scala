package it.vigtig.foopar.comm

trait Process {

  def ![T](msg: T,from:Int)
  def ?[T](size: Int = 0): T
  def sendSized[T](msg: T)
  def groupRcvSized[T](id: Symbol, fromRank: Int): T
  def groupRcv[T](fromRank: Int, size: Int = 0): T
  val globalRank: Int
  var commTime:Option[Long] = None

}