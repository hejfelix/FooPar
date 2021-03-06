package it.vigtig.foopar.examples

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Buffer
import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import akka.actor.Props
import it.vigtig.foopar.config.HostManager

case class MSG(m: String)

case class ActorList(al: Buffer[ActorRef])
case class ActorYou(a: ActorRef)
case class ActorDo(body: () => Unit)
case class Rank()

class FooParActor(val globalRank: Int) extends Actor {
  import context._

  implicit var me: ActorRef = self

  def receive(matcher: PartialFunction[Any, Unit]) {
    become(matcher)
    receive()
    //    unbecome()
  }

  def execute: Unit = {}

  var actors: Buffer[ActorRef] = ArrayBuffer()
  def receive = {
    case ActorList(al) =>
      actors = al
      println("Received actor list :)")
    case ActorDo(b) =>
      println("GOT PERFORM: "); b()
    case MSG(m) => println(m + " from " + Thread.currentThread().getId())
  }

}

object FooParAkkaTest extends HostManager {

  def me(a: ActorRef) = _nodes.find(_ == a).get

  //  val hm = new HostManager
  var system: Option[ActorSystem] = None
  var _myRank = -1
  private var _nodes = Buffer[ActorRef]()
  def main(args: Array[String]) {
    println("Reading file...")
    println("Hosts: " + countMap("./hostfile"))

    system = Some(startLocalSystem)
    val nodes = getNodes("./hostfile", system.get)
    nodes foreach (_ ! MSG("DUUDE"))
    nodes foreach (_ ! ActorList(nodes))
    _nodes = nodes
  }

  def startLocalSystem = {
    val system = ActorSystem("FooPar",
      ConfigFactory.load.getConfig("fooparserver"))

    val hostname = "127.0.0.1"
    val p = countProcs("./hostfile", hostname.toString)
    println(p, "started", hostname)
    for (i <- 0 until p) {
      system.actorOf(Props(new FooParActor(1337)), "oswald" + i)
    }
    println("Started all the actors :D")
    Thread.sleep(2000)
    system
  }

  def getNodes(file: String, system: ActorSystem) = {
    val cmap = countMap(file)
    val oswalds = Array[ActorRef]().toBuffer
    for ((hostname, count) <- cmap) {
      for (i <- 0 until count) {
        oswalds += system.actorFor(
          "akka://FooPar@" + hostname + ":2552/user/oswald" + i)
      }
    }
    oswalds
  }

}
