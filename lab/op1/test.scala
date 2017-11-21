package t
object main extends App {
  trait E
  case class Num(a:Int) extends E
  case class Atom(a:String) extends E
  case class Bin(e1:E,op:String,e2:E) extends E


  var tokens:List[E] = List(Num(1),Atom("*"),Num(3),Atom("+"),Num(4),Atom("+"),Num(5),Atom("*"),Num(6))
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
    case Nil => ptoken = Atom("nil")
    case x::xs => ptoken = x; tokens = xs
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
