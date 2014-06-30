package it.vigtig.foopar.test

import java.io.FileOutputStream
import it.vigtig.foopar.collection.DistGrid
import it.vigtig.foopar.FooParMain
import it.vigtig.foopar.FooParApp

object Mandelbrot extends FooParMain {

  val RES = 2500 //Per block
  def newApp(args: Array[String]) = new FooParApp {
    def run = {

      val sq = math.sqrt(worldSize).toInt //Assume P is a square
      val mandel = new Mandelbrot(100, RES * sq)
      DistGrid(0 until sq, 0 until sq) foreach {
        case x :: y :: Nil =>
          mandel.render(x * RES, y * RES, RES)
          pprintln(s"Done writing part $x, $y")
      }
    }
  }
}

class Mandelbrot(val level: Int, val res: Int) {

  case class Complex(a: Double, b: Double) {
    def +(that: Complex) = Complex(this.a + that.a, this.b + that.b)
    def *(that: Complex) = Complex(this.a * that.a - this.b * that.b, this.a * that.b + that.a * this.b)
    def abs = math.sqrt(this.a * this.a + this.b * this.b)
  }

  def pix(x0: Int, y0: Int) = {
    var z = Complex(0, 0)
    var c = Complex(-2.0 + x0 * 3.0 / res, -1.5 + y0 * 3.0 / res)
    for (i <- 0 until level if z.abs < 2)
      z = z * z + c
    if (z.abs < 2)
      (512 * z.abs).toInt
    else
      0
  }

  def render(xoff: Int = 0, yoff: Int = 0, size: Int = res): Unit = {
    val out = new FileOutputStream("mandel/" + xoff + "_" + yoff + "_" + size + "_mandelbrot.pgm")
    out.write(("P5\n" + size + " " + size + "\n255\n").getBytes())
    for {
      y0 <- yoff until yoff + size
      x0 <- xoff until xoff + size
    } {
      out.write(pix(x0, y0))
    }
    out.close()
  }
}