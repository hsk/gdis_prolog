package t
object main extends App {
  trait E
  case class Num(a:Int) extends E
  case class Atom(a:String) extends E
  case class Bin(e1:E,op:String,e2:E) extends E
  case class Cons(e1:E,e2:E) extends E
  case object N extends E
  def list(l:List[E]):E = l match {
    case Nil => N
    case x::xs => Cons(x,list(xs))
  }
  var tokens:E = list(List(Num(1),Atom("*"),Num(3),Atom("+"),Num(4),Atom("+"),Num(5),Atom("*"),Num(6)))
  var token:E = Atom("nil")
  var ptoken:E = Atom("nil")

  def infixs(op:E):Int = {
    op match {
      case Atom("+") => 10
      case Atom("-") => 10
      case Atom("*") => 2
      case Atom("/") => 2
      case _ => 10001
    }
  }
  def advance() : E = {
    token = ptoken
    tokens match {
    case N => ptoken = Atom("nil")
    case Cons(x,xs) => ptoken = x; tokens = xs
    }
    token
  }
  def exp(p:Int):E = {
    var t = advance()
    while(infixs(ptoken) <= p) {
      val Atom(op) = advance()
      t = Bin(t,op,exp(infixs(Atom(op))))
    }
    t
  }
  advance()
  println(exp(10000))
}
