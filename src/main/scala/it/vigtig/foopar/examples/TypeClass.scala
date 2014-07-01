package it.vigtig.foopar.examples

import it.vigtig.foopar.FooParApp
import it.vigtig.foopar.FooParMain

class MatrixIsNumeric extends Numeric[Matrix] with Ordering[Matrix] {
  def plus(x: Matrix, y: Matrix): Matrix = x + y
  def minus(x: Matrix, y: Matrix): Matrix = x - y
  def times(x: Matrix, y: Matrix): Matrix = x * y
  def negate(x: Matrix): Matrix = x * -1
  def fromInt(x: Int): Matrix = ???
  def toInt(x: Matrix): Int = ???
  def toLong(x: Matrix): Long = ???
  def toFloat(x: Matrix): Float = ???
  def toDouble(x: Matrix): Double = ???

  def compare(x: Matrix, y: Matrix) = x.toInt - y.toInt
}

object Matrix {
  implicit val num = new MatrixIsNumeric
}
case class Matrix(val data: Seq[Seq[Double]]) {

  val M = data.size
  val N = data(0).size

  def pieceWise(f: Double => Double) =
    new Matrix(data.map(_.map(f)))
  def pieceWise(f: (Double, Double) => Double)(that: Matrix) =
    new Matrix(
      (data, that.data).zipped.map((a, b) => (a, b).zipped.map(f)))

  def reduce(f: (Double, Double) => Double) = data.map(_.reduce(f)).reduce(f)

  //Matrix-Matrix operations
  def +(that: Matrix) = pieceWise(_ + _)(that)
  def -(that: Matrix) = pieceWise(_ - _)(that)
  def *(that: Matrix) = {
    val data = Array.ofDim[Double](M, N)
    for (i <- 0 until N; j <- 0 until that.M; k <- 0 until N)
      data(i)(j) = data(i)(j) + data(i)(k) * that.data(k)(j)
  }
  //Matrix-Scalar operations
  def *(scalar: Double) = pieceWise(_ * scalar)
  def /(scalar: Double) = pieceWise(_ / scalar)

  def equals(that: Matrix): Boolean =
    (data, that.data).zipped.map(_ == _).reduce(_ && _)
  override def toString = data.map("[" + _.mkString(",") + "]").mkString("\n")

}

object TypeClass extends FooParMain {
  def newApp(args: Array[String]) = new FooParApp {

    def run {

      val x = new Matrix(Seq(Seq(1, 2), Seq(3, 4)))
      val dSeq = Array.fill(worldSize)(x).toDistSeq

      for (res <- dSeq.sumD) {
        pprintln(res, " worldSize = " + worldSize, res == x * worldSize)
      }

    }

  }
}