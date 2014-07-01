package it.vigtig.foopar.config

import java.net.NetworkInterface
import sys.process.stringSeqToProcess
import java.net.InetAddress

trait HostManager {

  def countProcs(path: String, hostname: String) =
    if (new java.io.File(path).exists)
    io.Source.fromFile(path)
      .getLines
      .filter(_ != "")
      .map(_.split("\\s").head)
      .count(_ == hostname)
    else
      1

  def hosts(path: String) =
    if (new java.io.File(path).exists)
      io.Source.fromFile(path).getLines
        .filter(_ != "")
        .map(_.split("\\s"))
        .map(_.head)
        .toSet.toSeq
    else
      Seq(hostName)

  def hostIndex(path: String): Int =
    if (new java.io.File(path).exists)
      hosts(path).indexWhere(hostName == _)
    else
      0

  def indexOf(host: String, path: String) =
    if (new java.io.File(path).exists)
      hosts(path).indexWhere(host == _)
    else
      0

  def countMap(file: String) =
    hosts(file).map(x => x -> countProcs(file, x))

  def ipForHost(host: String) = InetAddress.getByName(host).getHostAddress().toString

  def getIP: String = {
    var ni = NetworkInterface.getByName("eth0");
    if (ni == null)
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