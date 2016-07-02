package gdis

import util.parsing.combinator._

object Parser extends RegexParsers {
  import Syntax._

  def list(args:List[T], tail:T):T = args match {
    case List()    => tail
    case x::xs     => Pred(".", List(x, list(xs, tail)))
  }

  def ATOM     = """[a-z][a-zA-Z0-9_]*""".r
  def NUMBER   = """[0-9]+(\.[0-9]+)?""".r       ^^ { case a     => a.toFloat }
  def STR      = "\""~"""([^"\\]|\\.)*""".r~"\"" ^^ { case a~b~c => StringContext.treatEscapes(b) }
  def VAR      = """[A-Z][a-zA-Z0-9_]*""".r
  def OP3       = """[+\-]""".r
  def OP2       = """[*/]""".r
  def OP1       = """=|is""".r
  def LPAREN   = "("
  def RPAREN   = ")"
  def LBRACKET = "["
  def RBRACKET = "]"
  def DOT      = "."
  def OR       = "|"
  def SEMI     = ";"
  def COMMA    = ","
  def IIF      = ":-"

  def query        = term~DOT                    ^^ { case a~b     => a }
  def seq          : Parser[List[T]]
                   = (sentence~seq               ^^ { case a~b     => a::b }).
                   | (sentence                   ^^ { case a       => List(a) })
  def sentence     = (term~DOT                   ^^ { case a~b     => Pred(":-", List(a, Atom("nop"))) }).
                   | (IIF~term~DOT               ^^ { case a~b~c   => Pred(":-", List(b)) }).
                   | (term~IIF~term~DOT          ^^ { case a~b~c~d => Pred(":-", List(a, c)) })
  def term         : Parser[T]
                   = (term1~SEMI~term            ^^ { case a~b~c   => Pred(";", List(a, c)) }).
                   | (term1                      ^^ { case a       => a })
  def term1        : Parser[T]
                   = (exp1~COMMA~term1           ^^ { case a~b~c   => Pred(",", List(a, c)) }).
                   | (exp1~term1                 ^^ { case a~b     => Pred(",", List(a, b)) }).
                   | (exp1                       ^^ { case a       => a })
  def exp1         : Parser[T]
                   = (exp2~OP1~exp1              ^^ { case a~b~c   => Pred(b, List(a, c)) }).
                   | (exp2                       ^^ { case a       => a })
  def exp2         : Parser[T]
                   = (exp3~OP2~exp2              ^^ { case a~b~c   => Pred(b, List(a, c)) }).
                   | (exp3                       ^^ { case a       => a })
  def exp3         : Parser[T]
                   = (exp~OP3~exp3               ^^ { case a~b~c   => Pred(b, List(a, c)) }).
                   | (exp                        ^^ { case a       => a })
  def exp          = (ATOM~LPAREN~exps~RPAREN    ^^ { case a~b~c~d => Pred(a, c) }).
                   | (ATOM                       ^^ { case a       => Atom(a) }).
                   | (VAR                        ^^ { case a       => Var(a, 0) }).
                   | (NUMBER                     ^^ { case a       => Number(a) }).
                   | (STR                        ^^ { case a       => Str(a) }).
                   | (LBRACKET~listbody~RBRACKET ^^ { case a~b~c   => b }).
                   | (LPAREN~term~RPAREN         ^^ { case a~b~c   => b })
  def exps         : Parser[List[T]]
                   = (exp~COMMA~exps             ^^ { case a~b~c   => a::c }).
                   | (exp                        ^^ { case a       => List(a) })
  def listbody     : Parser[T]
                   = (exps~OR~var_or_list        ^^ { case a~b~c   => list(a, c) }).
                   | (exps                       ^^ { case a       => list(a, Atom("[]")) })
  def var_or_list  = (VAR                        ^^ { case a       => Var(a, 0) }).
                   | (LBRACKET~listbody~RBRACKET ^^ { case a~b~c   => b })

  case class Parse_error(e:String) extends Exception

  def parseQuery(s:String):T = {
    parseAll(query, s) match {
      case Success(t,r) => t
      case e => throw Parse_error(e.toString)
    }
  }

  def parseSeq(s:String):List[T] = {
    parseAll(seq, s) match {
      case Success(d, r) => d
      case e => throw new Parse_error(e.toString)
    }
  }
}
