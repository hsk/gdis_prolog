package t
object main extends App {
  trait E
  case class Num(a:Int) extends E
  case class Atom(a:String) extends E
  case class Pred(op:String,e:List[E]) extends E
  def list(l:List[E]):E = l match {
    case Nil => Atom("nil")
    case x::xs => Pred("",List(x,list(xs)))
  }
  val tokens:E = list(List(Atom("a"),Atom("="),Atom("b"),Atom("="),Num(1),Atom("*"),Num(3),Atom("+"),Num(4),Atom("+"),Num(5),Atom("*"),Num(6)))

  def infix(op:E):Int = {
    op match {
      case Atom("+") => 10
      case Atom("-") => 10
      case Atom("*") => 2
      case Atom("/") => 2
      case _ => 10001
    }
  }
  def infixr(op:E):Int = {
    op match {
      case Atom("=") => 30
      case _ => 10001
    }
  }
  def exp_pre(p:Int,tokens:E):(E,E) = {
      tokens match {
        case Pred("",List(x,y)) => exp_infix(p,x,y)
      }
  }
  def exp_infix(p:Int,t:E,tokens:E):(E,E) = {
      tokens match {
        case Pred("",List(Atom(op),y)) if infix(Atom(op)) < p =>
          val (t2,ts2) = exp_pre(infix(Atom(op)),y)
          exp_infix(p, Pred(op,List(t,t2)),ts2)
        case Pred("",List(Atom(op),y)) if infixr(Atom(op)) <= p =>
          val (t2,ts2) = exp_pre(infixr(Atom(op)),y)
          exp_infix(p, Pred(op,List(t,t2)),ts2)
        case _ => (t,tokens)
      }
  }
  println(exp_pre(10000,tokens)._1)
}
