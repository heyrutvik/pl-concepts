package firstorder.exercises

object Exercise10 {

  /**
    * minimal language to demonstrate mutual recursion expression
    */

  type Env = firstorder.Env[Value]

  sealed trait Value
  case class Num(i: Int) extends Value
  case class Closure(f: String, p: String, body: Expr, fenv: Env) extends Value

  sealed trait Expr
  case class CstI(v: Int) extends Expr
  case class CstB(v: Boolean) extends Expr
  case class Var(s: String) extends Expr
  case class Prim(op: String, left: Expr, right: Expr) extends Expr
  case class If(cond: Expr, thenp: Expr, elsep: Expr) extends Expr
  case class LetFun(fs: List[FunDef], lbody: Expr) extends Expr
  case class Call(left: Expr, right: Expr) extends Expr

  case class FunDef(f: String, p: String, fbody: Expr)

  def eval(e: Expr)(env: Env): Int = e match {
    case CstI(i) => i
    case CstB(b) => if (b) 1 else 0
    case Var(s) =>
      firstorder.lookup(env)(s) match {
        case Num(i) => i
        case _ => sys.error(s"variable $s doesn't have number value")
      }
    case Prim(op, l, r) =>
      val l1 = eval(l)(env)
      val r1 = eval(r)(env)
      op match {
        case "*" => l1 * r1
        case "+" => l1 + r1
        case "-" => l1 - r1
        case "=" => if (l1 == r1) 1 else 0
        case "<" => if (l1 < r1) 1 else 0
        case _ => sys.error(s"$op primitive operation not supported")
      }

    case If(cond, thenp, elsep) =>
      if (eval(cond)(env) != 0)
        eval(thenp)(env)
      else
        eval(elsep)(env)

    case LetFun(fs, body) =>
      val e1 = fs.map { case FunDef(f, x, b) =>
        (f, Closure(f, x, b, env))
      }
      eval(body)(e1 ::: env)

    // naive implementation: adding `env` while evaluating body of function
    // one can also look for free variable in `b` and lookup those variables in `env` and add them in `env1`
    // or there could be more efficient ways to do that!
    case Call(Var(f), a) =>
      firstorder.lookup(env)(f) match {
        case c @ Closure(f, x, b, env1) =>
          val xv = eval(a)(env)
          eval(b)((x, Num(xv)) :: (f, c) :: env ::: env1) // TODO
      }

    case Call(_, _) => sys.error("Call: ")
  }

  /**
    * let isEven x = if x = 0 then true else isOdd x-1
    *     isOdd x = if x = 0 then false else isEven x-1
    * in isEven 3
    */
  val isEven = FunDef("isEven", "x", If(Prim("=", Var("x"), CstI(0)), CstB(true), Call(Var("isOdd"), Prim("-", Var("x"), CstI(1)))))
  val isOdd = FunDef("isOdd", "x", If(Prim("=", Var("x"), CstI(0)), CstB(false), Call(Var("isEven"), Prim("-", Var("x"), CstI(1)))))
  val mr = LetFun(List(isEven, isOdd), Call(Var("isEven"), CstI(3)))

  assert(eval(mr)(firstorder.emptyEnv) == 0)
}
