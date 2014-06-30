package it.vigtig.foopar.comm.pure

import com.typesafe.config.ConfigFactory
import akka.actor.ActorSystem
import akka.actor.Address
import akka.actor.ExtendedActorSystem
import akka.actor.Props
import it.vigtig.foopar.FooParApp
import it.vigtig.foopar.comm.Distributor
import it.vigtig.foopar.comm.Process
import it.vigtig.foopar.config.ArgsParser
import it.vigtig.foopar.config.HostManager
import java.net.InetAddress
import akka.actor.ActorRef
import akka.actor.Identify
import scala.concurrent.Await
import akka.util.Timeout
import akka.actor.ActorDSL._


case class Finish()
object PureDistributor extends HostManager {

  def confForIP(ip: String, port: String) = s"""
akka {
  actor {
    provider = "akka.remote.RemoteActorRefProvider"
  }
  remote { 
		  enabled-transports = ["akka.remote.netty.tcp"]
  
		  netty.tcp.hostname = "$ip" 
		  netty.tcp.port = $port
		  netty.tcp.message-frame-size = 20 MiB
		  //netty.tcp {
		//	  write-buffer-high-water-mark = 148000b
			//  send-buffer-size = 148000b
			 // receive-buffer-size = 148000b
		  //}
  }

  event-handlers = []
  loglevel=WARNING
  
}    

  
my-custom-dispatcher {
//    type = PinnedDispatcher
    executor = thread-pool-executor
    # 10 years should be enough
//    thread-pool-executor.keep-alive-time = 315360000s
    # note that disabling core timeout altogether doesn't work
    # until ticket 2856 is fixed
    thread-pool-executor.allow-core-timeout = off
	mailbox-type = "akka.dispatch.UnboundedDequeBasedMailbox"
}
  """
  
  val conf = confForIP(getIP, "2552")
  //  println("Loaded config: " + conf, "from " + hostName)
  private var system: Option[ActorSystem] = None
  def getSystem(machinefile: String) = synchronized {
    system match {
      case None =>
        system = Some(ActorSystem("FooPar" + indexOf(hostName, machinefile),
          ConfigFactory.parseString(conf))); system
      case Some(sys) => system
    }
  }
}

class PureDistributor(val index: Int, val worldSize: Int) extends Distributor
  with ArgsParser
  with HostManager {

  val globalRank: Int = index //Calculate from config file+hostfile!
  private var mySystem: Option[ActorSystem] = None
  private var myProcess: Option[Process] = None
  var args: Array[String] = Array()
  var machinefile = ""

  def initialize(args: Array[String]) = {
    this.args = args
    machinefile = args.getValueOr("-machinefile", "machinefile")
    mySystem = PureDistributor.getSystem(machinefile)
  }

  var processes = Array[Process]()
  def run[T <: FooParApp](appFactory: (Array[String]) => T) {

    val myHostName = java.net.InetAddress.getLocalHost().getHostName();

    //    println(myHostName)

    val hostNames = hosts(machinefile)

    mySystem match {
      case None => sys.error("Something went wrong, system is offline...")
      case Some(sys) =>

        //Instantiate my own actor
        val myActor = sys.actorOf(Props(new FooParActor(this)).withDispatcher("my-custom-dispatcher"), "oswald" + globalRank)
        myProcess = Some(FooParAkkaProcess(myActor, index,sys,myActor,index))
        Thread.sleep(1000)
        val npp = args.getValueOr("-fpppn", "1").toInt

        val myaddr = sys.asInstanceOf[ExtendedActorSystem].provider.getExternalAddressFor(Address("akka.tcp", "", "0.0.0.0", 0))
        //        println("My external addr: " + myaddr)

        //Look up other actors
        val ab = Array[Process]().toBuffer
        var nextRank = 0

        for (host <- hosts(machinefile)) {
          for (i <- nextRank until nextRank + npp) {
            var nextHost = InetAddress.getByName(host).getHostAddress()
            val name = (if (host == hostName) myaddr.get else "akka.tcp://FooPar" + indexOf(host, machinefile) + "@" + nextHost + ":2552") + "/user/oswald" + i
            //            println(hostName + ", " + globalRank + " looking up: " + name)
            ab += FooParAkkaProcess(sys.actorFor(name), i,sys,myActor,index)
          }
          nextRank += npp
        }
        processes = ab.toArray

        val spmd = appFactory(args)
        spmd.init(args)
        spmd._distributor = Some(this)

        //Run user defined SPMD program
        spmd.run

    }
  }

  def finish {
    val ID = 'finish
    val ppn = args.getValueOr("-fpppn", "1").toInt
    if (globalRank % ppn == 0) {
      Thread.sleep(5000)
      println(globalRank + ": shutting down system " + mySystem)
      mySystem foreach (_.shutdown)
      System.exit(0)
    } else {
    }
  }
}