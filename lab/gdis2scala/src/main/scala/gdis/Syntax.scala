package gdis

object Syntax {

  type V = (String, Int)

  sealed trait T
  case class Atom(_1: String) extends T
  case class Num(_1: Float) extends T
  case class Str(_1: String) extends T
  case class Pred(_1: String, _2:List[T]) extends T
  case class Var(_1: V) extends T

  def show(t:T):String = t match {
    case Atom(n)      => n
    case Num(v)       => v.toString
    case Str(v)       => v
    case Pred(".", _) => "[%s]".format(show_list(t))
    case Pred(n, xs)  => "%s(%s)".format(n, xs.map(show).mkString(", "))
    case Var((n, l))  => "%s_%d".format(n, l)
  }

  def show_list(t:T):String = t match {
    case Pred(".", List(t1, Atom("[]")))      => show(t1)
    case Pred(".", List(t1, t2@Pred(".", _))) => show(t1) + show_list(t2)
    case Pred(".", List(t1, t2))              => show(t1) + "|" + show(t2)
    case t1                                   => show(t1)
  }
}
