package firstorder.exercises

object Exercise6 {

  /**
    * adding tuple in `Exercise3`
    */

  type Env = firstorder.Env[Value]

  // TupV added to Value type
  sealed trait Value
  case class Num(i: Int) extends Value
  case class TupV(vs: List[Value]) extends Value
  case class Closure(f: String, ps: List[String], fbody: Expr, fenv: Env) extends Value

  /**
    * change abstract syntax to accommodate tuple and its selector
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
  // tuple definition
  case class Tup(vs: List[Expr]) extends Expr
  // tuple selector
  case class Sel(i: Int, e: Expr) extends Expr      // index `i` starts from 1

  /**
    * evaluator: case added to handle tuple and selector
    *
    * note: return type of `eval` is now `Value` because it can be
    *       return integer as well as tuple value.
    */
  def eval(e: Expr)(env: Env): Value = e match {
    case CstI(i) => Num(i)
    case CstB(b) => Num(if (b) 1 else 0)

    case Var(s) =>
      firstorder.lookup(env)(s) match {
        case n: Num => n
        case t: TupV => t
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
  }

  // let s = (1,2,1+2) in #1(s) end
  val t1 =
    Let("s",
      Tup(List(CstI(1), CstI(2), Prim("+", CstI(1), CstI(2)))),
      Sel(1, Var("s"))
    )
  assert(eval(t1)(firstorder.emptyEnv) == Num(1))

  // let s = ((1,2,1+2),2,1+2) in #1(s) end
  val t2 =
    Let("s",
      Tup(List(
        Tup(List(CstI(1), CstI(2), Prim("+", CstI(1), CstI(2)))),
        CstI(2),
        Prim("+", CstI(1), CstI(2)))
      ),
      Sel(1, Var("s"))
    )
  assert(eval(t2)(firstorder.emptyEnv) == TupV(List(Num(1), Num(2), Num(3))))
}
