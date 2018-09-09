package firstorder.exercises

object Exercise8 {

  /**
    * adding list and match expression in `Exercise6`
    */

  type Env = firstorder.Env[Value]

  // TupV added to Value type
  sealed trait Value
  case class Num(i: Int) extends Value
  case class TupV(vs: List[Value]) extends Value
  case class ListV(ls: List[Value]) extends Value
  case class Closure(f: String, ps: List[String], fbody: Expr, fenv: Env) extends Value

  /**
    * change abstract syntax to accommodate list and match expression
    */
  sealed trait Expr
  case class CstI(v: Int) extends Expr
  case class CstB(v: Boolean) extends Expr
  case class Var(s: String) extends Expr
  case class Let(s: String, rhs: Expr, body: Expr) extends Expr
  case class Prim(op: String, left: Expr, right: Expr) extends Expr
  case class If(cond: Expr, thenp: Expr, elsep: Expr) extends Expr
  case class LetFun(f: String, ps: List[String], fbody: Expr, lbody: Expr) extends Expr
  case class Call(f: Expr, as: List[Expr]) extends Expr
  case class Tup(vs: List[Expr]) extends Expr
  case class Sel(i: Int, e: Expr) extends Expr
  // added new cases.. nil, cons and match
  case object CstN extends Expr
  case class ConC(h: Expr, t: Expr) extends Expr
  case class Match(w: Expr, bcase: Expr, rcase: RCase) extends Expr

  // helper class for recursive case in match expression
  case class RCase(h: String, t: String, e: Expr)

  /**
    * evaluator: case added to handle list and match
    */
  def eval(e: Expr)(env: Env): Value = e match {
    case CstI(i) => Num(i)
    case CstB(b) => Num(if (b) 1 else 0)

    case Var(s) =>
      firstorder.lookup(env)(s) match {
        case n: Num => n
        case t: TupV => t
        case l: ListV => l
        case _ => sys.error("Var: ")
      }

    case Let(s, rhs, body) =>
      val sv = eval(rhs)(env)
      eval(body)((s, sv) :: env)

    case Prim(op, l, r) =>
      val Num(l1) = eval(l)(env)
      val Num(r1) = eval(r)(env)
      val ret = op match {
        case "*" => l1 * r1
        case "+" => l1 + r1
        case "-" => l1 - r1
        case "=" => if (l1 == r1) 1 else 0
        case "<" => if (l1 < r1) 1 else 0
        case _ => sys.error(s"$op is not primitive operation")
      }
      Num(ret)

    case If(cond, thenp, elsep) =>
      val cv = eval(cond)(env)
      if (cv != 0) eval(thenp)(env) else eval(elsep)(env)

    case LetFun(f, ps, fbody, lbody) if ps.nonEmpty =>
      val c = Closure(f, ps, fbody, env)
      eval(lbody)((f, c) :: env)

    case Call(Var(f), as) if as.nonEmpty =>
      firstorder.lookup(env)(f) match {
        case c @ Closure(f, ps, body, fenv) if ps.nonEmpty =>
          val nenv = ps.zip(as).map {
            case (s,e) => (s, eval(e)(env))
          }
          val bodyEnv = nenv ::: List((f, c)) ::: fenv
          eval(body)(bodyEnv)
        case _ => sys.error("Call: ")
      }

    case Call(_, _) => sys.error(s"function application error")

    case Tup(vs) => TupV(vs.map(v => eval(v)(env)))

    case Sel(i, e) =>
      eval(e)(env) match {
        case TupV(vs) if i < vs.length => vs(i-1)
        case _ => sys.error("Sel: ")
      }

    case CstN => ListV(Nil)

    case ConC(h, t) =>
      (eval(h)(env), eval(t)(env)) match {
        case (n: Num, ListV(ys)) => ListV(n :: ys)
        case (t: TupV, ListV(ys)) => ListV(t :: ys)
        // TODO: not sure if it's right !!!
        // how to handle list of list case ???
        case (l1: ListV, l2: ListV) => ListV(List(l1) ::: List(l2))
        case _ => sys.error("ConC: ")
      }

    case Match(w, bcase, RCase(h, t, rcase)) =>
      w match {
        case Var(l) =>
          firstorder.lookup(env)(l) match {
            case ListV(vs) => vs match {
              case Nil => eval(bcase)(env)
              case h1 :: t1 =>
                eval(rcase)((h, h1) :: (t, ListV(t1)) :: env)
            }
          }
        case _ => sys.error("match only supported with variable")
      }
  }

  // list of list.. need to test this using case defined in `eval`
  val ll = ConC(ConC(CstI(1), CstN), ConC(ConC(CstI(2), CstN), ConC(ConC(CstI(3), CstN), CstN)))

  val l1 = ConC(CstI(1), ConC(CstI(2), ConC(Tup(List(CstI(1), CstI(2))), CstN)))
  assert(eval(l1)(firstorder.emptyEnv) == ListV(List(Num(1), Num(2), TupV(List(Num(1), Num(2))))))

  val ls = ConC(CstI(1), ConC(CstI(2), CstN))
  val sum =
    LetFun(
      "sum",
      List("l"),
      Match(
        Var("l"),                                                              // match list `l`
        CstI(0),                                                               // []     -> 0
        RCase("h", "t", Prim("+", Var("h"), Call(Var("sum"), List(Var("t"))))) // h :: t -> h + f(t)
      ),
      Call(Var("sum"), List(ls))
    )

  assert(eval(sum)(firstorder.emptyEnv) == Num(3))
}
