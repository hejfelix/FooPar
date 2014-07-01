
import java.io.PrintStream
import it.vigtig.foopar.FooParApp
import java.io.OutputStream
import org.scalacheck.Properties
import org.scalacheck.Gen
import org.scalacheck.Test
import org.scalacheck.Test.Parameters
import scala.util.Random

abstract class FooParTestApp(name: String) extends Properties(name) with FooParApp {

  val out = System.out;
  def DEBUG_PRINT = globalRank == 0

  case class OutWrap(os: OutputStream) extends PrintStream(os) {
    override def println(s: String) = { if (DEBUG_PRINT) super.println(s) }
    override def print(s: String) = { if (DEBUG_PRINT) super.print(s) }
  }

  val fpOut: PrintStream = new OutWrap(out)
  Console.setOut(fpOut)

  def run() = {
    if (globalRank == 0)
      println(s"""Starting test suite "${name}" with ${worldSize} processing elements""")

    val prm: Parameters = new Test.Parameters.Default {
      override val minSuccessfulTests: Int = 100
      override val rng = new Random(1337)
    }

    this.check(prm)
  }

}