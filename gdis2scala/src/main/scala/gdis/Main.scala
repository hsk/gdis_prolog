package gdis

import scala.annotation.tailrec

object Main {
  import Syntax._
  import Prolog._

  val welcome = "Beautiful Japanese Prolog Interpreter"

  def parse(str:String):T = Parser.parseQuery(str)

  def help () {
    List("q"->"quit", "l"->"list", "h"->"help").foreach {
      case (k,v) => printf("%s\t%s\n", k, v)
    }
  }

  @tailrec def repl (d:D) {
      print("? ")
      scala.io.StdIn.readLine() match {
      case "q"   =>
      case "l"   => d.foreach { t => println(Syntax.show(t))}; repl(d)
      case "h"   => help(); repl(d)
      case "t"   => trace = !trace
                    printf("Tracing %s.\n", if (trace) "on" else "off")
                    repl(d)
      case line  => repl(try { process(d, parse(line)) }
                         catch { case Parser.Parse_error(e) => println(e); d })
      }
  }

  def main(args:Array[String]) {
    try {
      var db = consult1(Array(), Atom("lib/initial.pl")) // load files

      @tailrec def optParse(args:List[String]) {
        val regopt = "(-.*)".r
        args match {
          case List() =>
          case "-t"::xs => trace = !trace; optParse(xs)
          case regopt(x)::_ => println("Usage: bjpl [-t] filename1 filename2 ..."); throw Halt
          case x::xs => db = consult1(db, Atom(x)); optParse(xs)
        }
      }
      optParse(args.toList)

      println("-" * welcome.length)
      println(welcome)
      println("-" * welcome.length)
      help()
      repl(db)
    } catch {
      case Halt =>
    }
  }
}
