import scala.tools.nsc
import nsc.reporters._
import nsc.util._
import nsc.interactive._
import nsc._

object Compiler {
  val settings = new Settings()
  settings.Ytyperdebug.value = true
  settings.debug.value = true

  val reporter = new ConsoleReporter(settings)
  lazy val compiler = new interactive.Global(settings, reporter)
}

class Symbols extends StatefulSnippet {
  def test = <div>{S.param("name")}</div>
}
