package it.vigtig.foopar.comm.pure

import scala.concurrent.Await
import akka.actor.ActorRef
import akka.actor.actorRef2Scala
import akka.pattern.ask

import akka.util.Timeout
import it.vigtig.foopar.comm.Process
import akka.actor.ActorSystem
import akka.actor.ActorDSL._

case class FooParAkkaProcess(val aref: ActorRef, grank: Int, sys: ActorSystem, val localActor: ActorRef,val sender:Int) extends Process {
  implicit val timeout = Timeout(12000000)
  implicit val asys = sys
  implicit val box = inbox()
  commTime = Some(0l)
  def groupRcv[T](fromRank: Int, size: Int = 0) = {
    val start = System.nanoTime
    val res = Await.result(localActor ? GroupReceive(fromRank), timeout.duration).asInstanceOf[T]
    commTime = commTime.map(_ + (System.nanoTime() - start))
    res
  }
  def ?[T](size: Int = 0) = {
    val start = System.nanoTime
    val res = Await.result(aref ? Receive, timeout.duration).asInstanceOf[T]
    commTime = commTime.map(_ + (System.nanoTime() - start))
    res
  }
  def ![T](msg: T,from:Int) {
    val start = System.nanoTime
    val res = aref ! GroupMsg(from,msg)
    commTime = commTime.map(_ + (System.nanoTime() - start))
    res
  }

  def sendSized[T](msg: T) = ???
  def groupRcvSized[T](id: Symbol, fromRank: Int): T = groupRcv(fromRank)

  val globalRank = grank
}