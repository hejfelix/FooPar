package it.vigtig.foopar.comm.pure

import akka.actor.Stash
import it.vigtig.foopar.comm.Distributor
import akka.actor.Actor
import akka.actor.ActorRef
import scala.collection.mutable.Queue
import java.nio.ByteBuffer

case class Receive()
case class GroupReceive(fromRank: Int)
case class GroupMsg[T](myRank: Int, msg: T)
case class HeartBeat()
class FooParActor(val distributor: Distributor) extends Actor
  with Stash {
  import context._

  implicit var me: ActorRef = self
  val globalRank: Int = distributor.globalRank
  var queue: Queue[GroupMsg[_]] = Queue()

  def receive = {
    case gm @ GroupMsg(id, x) =>
      queue.enqueue(gm)
      unstashAll()
    case bytes: Array[Byte] =>
      unstashAll()
    case HeartBeat => unstashAll()
    case GroupReceive(fromRank) =>
      unstashAll()
      val ID = fromRank
      val msg = queue.dequeueFirst { _.myRank == ID }
      msg match {
        case Some(GroupMsg(ID, x)) => sender ! x
        case Some(x) => sys.error("weird case msg")
        case None => stash(); self ! HeartBeat
      }
    case x => sys.error("bad msg: " + x)
  }

}