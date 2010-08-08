package code.snippet

import net.liftweb
import liftweb._
import liftweb.util._
import liftweb.http._
import SHtml._
import Helpers._

import scala.xml._

import scala.tools.nsc._
import reporters._
import scala.tools.nsc.util._
import interactive._

object Compiler {
  val settings = new Settings()
  settings.Ytyperdebug.value = true
  settings.debug.value = true
  settings.classpath.value = "lib/scala-library.jar"

  val reporter = new ConsoleReporter(settings)
  lazy val compiler = {
    val res = new interactive.Global(settings, reporter)
    val src = new BatchSourceFile("<test>","object Test {val x = 12+15}")
    val tree = res.typedTree(src, true)
    res
  }

  val knownSymbols = new scala.collection.mutable.HashMap[String, compiler.Symbol]
  def sym2name(sym: compiler.Symbol): String = {
    val name = sym.toString+sym.fullName
    //if (knownSymbols.contains(name) && knownSymbols(name) != sym)
    //  throw new RuntimeException("Double symbol "+name)

    knownSymbols(name) = sym
    name      
  }
  def name2sym(name: String): Option[compiler.Symbol] = {
    knownSymbols.get(name)
  }
}

class Symbols {
  import Compiler.compiler._

  def symbolLink(sym: Symbol) = {
    val name = Compiler.sym2name(sym)
    link("/symbol?name="+name, ()=>(), Text(sym.fullName))
  }
  def isValidSymbol(sym: Symbol): Boolean = !sym.name.toString.contains("$")

  def test = <div>{S.param("name")}</div>

  def info(xhtml: NodeSeq) = {
    infoBlock(xhtml, S.param("name").flatMap(Compiler.name2sym).openOr(definitions.RootClass))
  }

  def typeBlock(xhtml: NodeSeq, tpe: Type): NodeSeq = {
    def members(xhtml: NodeSeq): NodeSeq =
      tpe.members.filter(isValidSymbol).flatMap( sym =>        
        bind("member", xhtml, 
             "name" -> symbolLink(sym),
             "type" -> sym.kindString
           )
      )

    bind("type", xhtml,
         "name" -> tpe.toString,
         "members" -> members(chooseTemplate("type", "members", xhtml))
       )
  }

  def infoBlock(xhtml: NodeSeq, sym: Symbol) = {
    bind("symbol", xhtml,
         "name" -> sym.name.toString,
         "fullName" -> sym.fullName,
         "owner" -> symbolLink(if (sym == NoSymbol) NoSymbol else sym.owner),
         "companionClass" -> symbolLink(sym.companionClass),
         "companionModule" -> symbolLink(sym.companionModule),
         "privateWithin" -> symbolLink(sym.privateWithin),
         "superClass" -> symbolLink(sym.superClass),
         "typeName" -> sym.tpe.toString,
         "type" -> typeBlock(chooseTemplate("symbol","type",xhtml), sym.tpe)
       )
  }
}
