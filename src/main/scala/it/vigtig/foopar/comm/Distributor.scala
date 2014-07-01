package it.vigtig.foopar.comm

import it.vigtig.foopar.FooParApp

trait Distributor {
  def globalRank:Int
  def worldSize:Int
  def initialize(args: Array[String])
  def run[T <: FooParApp](appFactory: Array[String] => T)
  def finish()
  def processes:Array[Process]
}