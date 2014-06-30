package it.vigtig.foopar.config

import java.net.NetworkInterface
import sys.process.stringSeqToProcess
import java.net.InetAddress

trait HostManager {

  def countProcs(file: String, hostname: String) =
    io.Source.fromFile(file)
      .getLines
      .filter(_ != "")
      .map(_.split("\\s").head)
      .count(_ == hostname)

  def hosts(file: String) =
    io.Source.fromFile(file).getLines
      .filter(_ != "")
      .map(_.split("\\s"))
      .map(_.head)
      .toSet.toSeq

  def hostIndex(file: String) = hosts(file).indexWhere(hostName == _)
  def indexOf(host: String, file: String) = hosts(file).indexWhere(host == _)

  def countMap(file: String) =
    hosts(file).map(x => x -> countProcs(file, x))

  def ipForHost(host:String) = InetAddress.getByName(host).getHostAddress().toString
    
  def getIP: String = {
    var ni = NetworkInterface.getByName("eth0");
    if(ni==null)
      ni = NetworkInterface.getByName("wlan0")
    val inetAddresses = ni.getInetAddresses();

    while (inetAddresses.hasMoreElements()) {
      val ia = inetAddresses.nextElement();
      if (!ia.isLinkLocalAddress()) {
        return ia.getHostAddress()
      }
    }
    sys.error("Couldn't find IP")
  }

  def hostName = Seq("bash", "-c", "hostname").!!.dropRight(1)
  //  def hostName = java.net.InetAddress.getLocalHost().getHostName()
}