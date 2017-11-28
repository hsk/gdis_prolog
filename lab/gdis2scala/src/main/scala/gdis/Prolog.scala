package gdis

import scala.annotation.tailrec

object Prolog {
  import Syntax._

  type E = Map[V, T]
  case object Halt extends Exception

  def deref(e:E, t:T):T = t match {
    case Pred(n, ts) => Pred(n, ts.map{deref(e,_)})
    case Var(v)      => try { deref(e, e(v)) } catch {case _:Throwable => t}
    case t1          => t1
  }

  def show(e:E) =
    e.toList.foldLeft(List[String]()) {case (ls, ((n, l), t)) =>
      if (l > 0) ls else (n + "=" + Syntax.show(deref(e, t))) :: ls
    }.mkString("\n")

  type R = Option[E]
  def unify(r:R, t:T, t2:T):R = r match {
    case None => None
    case Some(e) =>
      def bind(t:T, v:V, t2:T):R =
        try {
          e(v) match {
            case t3 @ Var(v) => if(t == t3) None else bind(t, v, t2)
            case t3          => if(t2== t3) r    else mgu(t3, t2)
          }
        } catch {case _:Throwable => Some(e + (v -> t2) ) }
      def mgu(t:T, t2:T):R = (t, t2) match {
        case (        t ,      Var(v2)) => bind(t2, v2, t)
        case (    Var(v),          t2 ) => bind(t, v, t2)
        case (Pred(x, g), Pred(x2, g2)) => if (x != x2) None else
                                           if (g.length != g2.length) None else
                                           g.zip(g2).foldLeft(r){case(r,(t,t2))=>unify(r,t,t2)}
        case (        t ,          t2 ) => if (t == t2) r else None
      }
      mgu(t, t2)
  }

  type G = List[T]           // goals
  type D = Array[T]          // database
  type I = Int               // index
  type S = List[(G, E, I)]   // stack
  type M = (G, D, I, S)      // gdis machine

  sealed trait Res[A,B]
  case class Fail[A,B](a:A) extends Res[A,B]
  case class Succ[A,B](b:B) extends Res[A,B]

  var trace = false

  def e(s:S):E = s match {
    case List() => Map()
    case (_, e, _)::_ => e
  }
  def pop(m:M):Res[D,M] = m match {
    case (_,  d, _, (g, _, i)::s) => Succ((g, d, i, s))
    case (_,  d, _,       List()) => Fail(d)
  }
  def uni(m:M, s:S, t:T, t2:T):Res[D,M] =
    (unify(Some(e(s)), t, t2), m) match {
      case (Some(e), (_ :: g, d, _, (sg, _, i) :: s)) => Succ((g, d, -1, (sg, e, i) :: s))
      case (_, m) => pop(m)
    }
  def eval(e:E,t:T):Float = t match {
    case Num(i) => i
    case Pred("+", List(x,y)) => eval(e, x) + eval(e, y)
    case Pred("*", List(x,y)) => eval(e, x) * eval(e, y)
    case Pred("-", List(x,y)) => eval(e, x) - eval(e, y)
    case Pred("/", List(x,y)) => eval(e, x) / eval(e, y)
    case t => throw new Exception ("unknown term " + Syntax.show(t))
  }
  def write1(e:E, t:T) { print(Syntax.show(deref(e, t))) }

  def assert1(d:D,t:T):D = t match {
    case Pred(":-", List(t)) => process(d, t)
    case t                   => d ++ Array(t)
  }

  def consult1(d:D, t:T):D = {
    val filename = Syntax.show(t)
    if (trace) println("Loading " + filename)
    val str = scala.io.Source.fromFile(filename).getLines.mkString("\n")
    try {
      val seq = Parser.parseSeq(str)
      seq.foldLeft(d) { case (d, t) => assert1(d, t) }
    } catch {
      case Parser.Parse_error(e) => println(e); d
    }
  }

  def solve(m:M):Res[D,M] = {
    @tailrec def step(r:Res[D,M]):Res[D,M] = r match {
    case Fail(d)                => Fail(d)
    case Succ(m @ (g, d, i, s)) =>
      if(trace) printf("i=%d g=[%s],e=[%s],s=%d\n",
        i, g.map{Syntax.show}.mkString("; "), show(e(s)), s.length)
      m match {
        case (List(), d, i, s) => Succ(m)
        case (g, d, -2, s) => Fail(d)
        case (Atom("halt") :: g, d, -1, s) => throw new Exception("halt")
        case (Atom("nop") :: g, d, -1, s) => step(Succ((g, d, -1, s)))
        case (Atom("!") :: g, d, -1, (g2, e, _) :: s) => step(Succ((g, d, -1, (g2, e, -2) :: s)))
        case (Pred(",", List(u, v)) :: g, d, -1, s) => step(Succ(u :: v :: g, d, -1, s))
        case (Pred(";", List(u, v)) :: g, d, -1, s) => step(Succ(u :: g, d, -1, (v :: g, e(s), -1) :: s))
        case (Pred("=", List(u, v)) :: g, d, -1, s) => step(uni(m, s, u, v))
        case (Pred("is", List(u, v)) :: g, d, -1, s) => step(uni(m, s, u, Num(eval(e(s), deref(e(s), v)))))
        case (Pred("assert", List(t)) :: g, d, -1, s) => step(Succ((g, assert1(d, deref(e(s), t)), i, s)))
        case (Pred("write", List(t)) :: g, d, -1, s) => write1(e(s), t); step(Succ(g, d, -1, s))
        case (Pred("consult", List(t)) :: g, d, -1, s) => step(Succ((g, consult1(d, deref(e(s), t)), i, s)))
        case (g, d, -1, s) => step(Succ((g, d, 0, s)))
        case (t :: g, d, i, s) =>
          if (i >= d.length) step(pop(m)) else
          d(i) match {
            case Pred(":-", List(t2, t3)) =>
              def gen_t(t: T): T = t match {
                case Pred(n, ts) => Pred(n, ts.map { gen_t })
                case Var((n, _)) => Var((n, s.length + 1))
                case t => t
              }
              unify(Some(e(s)), t, gen_t(t2)) match {
                case None    => step(Succ((t :: g, d, i + 1, s)))
                case Some(e) => step(Succ((gen_t(t3) :: g, d, -1, (t :: g, e, i + 1) :: s)))
              }
            case t => printf("Database is broken. %s\n", Syntax.show(t)); Fail(d)
          }
      }
    }
    step (m match {
      case (List(), _, _, _) => pop(m)
      case _ => Succ(m)
    })
  }

  def process(d:D, t:T):D = {
    @tailrec def prove(m:M):D = solve(m) match {
      case Fail(d) => println("No.\n"); d
      case Succ(m @ (g, d, i, s)) =>
        println(show(e(s)))
        if (s == List() || i == -2) {println("Yes."); d} else {
          print("More y/n")
          if ("y" == scala.io.StdIn.readLine()) prove(m) else d
        }
    }
    prove((List(t), d, -1, List((List(),Map(),-2))))
  }
}
