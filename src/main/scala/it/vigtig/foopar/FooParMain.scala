package it.vigtig.foopar

import it.vigtig.foopar.comm.pure.PureDistributor
import it.vigtig.foopar.config.ArgsParser
import it.vigtig.foopar.config.HostManager
import java.lang.management.ManagementFactory
//import it.vigtig.foopar.comm.mpjexpress.MPJExpressDistributor

trait FooParMain extends ArgsParser
  with HostManager {
  self =>
  def newApp(args: Array[String]): FooParApp
  var apps: List[FooParApp] = Nil
  var worldSize = 1 //Pretend we read this from config!!

  private val timeBean = ManagementFactory.getThreadMXBean()
  if (!timeBean.isCurrentThreadCpuTimeSupported())
    sys.error("Current-Thread CPU-Time not supported...")
  def time() = timeBean.getCurrentThreadUserTime()

  def main(args: Array[String]): Unit = {
    val BACKEND = args.getValueOr("-backend", "akka")
    val machinefile = args.getValueOr("-machinefile", "machinefile")
    val ppn = args.getValueOr("-fpppn", "1").toInt
    worldSize = args.getValueOr("-fpnp", (ppn * (hosts(machinefile).size)).toString).toInt
    var rank = hostIndex(machinefile)
    /*
     * TODO: should read config and pick distributor!! 
     * (but reflection is still kinda yucky...)
     */

    val distributors = rank * ppn until (rank * ppn + ppn) map (i =>

      new PureDistributor(i, worldSize))
    distributors foreach { dist =>
      val t = new Thread {
        override def run {
          dist.initialize(args)
          //            val startTime = time()
          if (dist.globalRank < worldSize) //Only start selected number of processes
            dist.run(newApp _)
          //            println(dist.globalRank + ": threadusertime taken for run = " + (time() - startTime) / 1000000000d)
          dist.finish
        }
      }
      t.start
    }
    //}

  }

}
