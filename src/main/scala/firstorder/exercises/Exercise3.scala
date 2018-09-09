package firstorder.exercises

object Exercise3 {

  type Env = firstorder.Env[Value]

  sealed trait Value
  case class Num(i: Int) extends Value
  case class Closure(f: String, ps: List[String], fbody: Expr, fenv: Env) extends Value

  /**
    * change abstract syntax to accommodate functions with multiple arguments
    */
  sealed trait Expr
  case class CstI(v: Int) extends Expr
  case class CstB(v: Boolean) extends Expr
  case class Var(s: String) extends Expr
  case class Let(s: String, rhs: Expr, body: Expr) extends Expr
  case class Prim(op: String, left: Expr, right: Expr) extends Expr
  case class If(cond: Expr, thenp: Expr, elsep: Expr) extends Expr
  // multiple param list `ps` for function definition
  case class LetFun(f: String, ps: List[String], fbody: Expr, lbody: Expr) extends Expr
  // multiple argument `as` to function call
  case class Call(f: Expr, as: List[Expr]) extends Expr

  /**
    * evaluator for language with multiple argument list to function
    */
  def eval(e: Expr)(env: Env): Int = e match {
    case CstI(i) => i
    case CstB(b) => if (b) 1 else 0

    case Var(s) =>
      firstorder.lookup(env)(s) match {
        case Num(v) => v
        case _ => sys.error("Var: ")
      }

    case Let(s, rhs, body) =>
      val sv = eval(rhs)(env)
      val nenv = (s, Num(sv)) :: env
      eval(body)(nenv)

    case Prim(op, l, r) =>
      val l1 = eval(l)(env)
      val r1 = eval(r)(env)
      op match {
        case "*" => l1 * r1
        case "+" => l1 + r1
        case "-" => l1 - r1
        case "=" => if (l1 == r1) 1 else 0
        case "<" => if (l1 < r1) 1 else 0
        case _ => sys.error(s"$op is not primitive operation")
      }

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
            case (s,e) => (s, Num(eval(e)(env)))
          }
          val bodyEnv = nenv ::: List((f, c)) ::: fenv
          eval(body)(bodyEnv)
        case _ => sys.error("Call: ")
      }

    case Call(_, _) => sys.error(s"function application error")
  }

  // sum function
  val sum = LetFun("sum", List("x", "y"), Prim("+", Var("x"), Var("y")), Call(Var("sum"), List(CstI(1), CstI(1))))
  assert(eval(sum)(firstorder.emptyEnv) == 2, s"$sum != 2")
}
