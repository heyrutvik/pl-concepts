package firstorder.fun

/**
  * "abstract syntax" representation of first-order functional language
  *
  * we added `If`, `LetFun` and `Call` in expression language.
  */
sealed trait Expr
case class CstI(v: Int) extends Expr
case class CstB(v: Boolean) extends Expr
case class Var(s: String) extends Expr
case class Let(s: String, rhs: Expr, body: Expr) extends Expr
case class Prim(op: String, left: Expr, right: Expr) extends Expr

// new cases added here for first-order language
case class If(cond: Expr, thenp: Expr, elsep: Expr) extends Expr
case class LetFun(s: String, p: String, fbody: Expr, lbody: Expr) extends Expr
case class Call(left: Expr, right: Expr) extends Expr

object Fun {

  // environment representation.. maps name to corresponding `Value`
  type Env = firstorder.Env[Value]

  /**
    * variable name maps to integer
    * function name maps to closure
    * in environment above
    */
  sealed trait Value
  case class Num(i: Int) extends Value
  case class Closure(s: String, p: String, fbody: Expr, fenv: Env) extends Value

  // variable scope
  sealed trait Scope
  case object Static extends Scope
  case object Dynamic extends Scope

  // evaluating the functional language
  private def eval(e: Expr)(env: Env)(implicit scope: Scope): Int = e match {
    case CstI(i) => i

    case CstB(b) => if (b) 1 else 0

    case Var(x) =>
      firstorder.lookup(env)(x) match {
        case Num(i) => i
        case _ => sys.error(s"eval Var")
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
        case _ => sys.error(s"unknown primitive $op")
      }

    case Let(s, rhs, body) =>
      val v = eval(rhs)(env)
      eval(body)((s, Num(v)) :: env)

    // new cases added here for first-order language
    case If(cond, thenp, elsep) =>
      if (eval(cond)(env) != 0) eval(thenp)(env) else eval(elsep)(env)

    case LetFun(f, x, fbody, letbody) =>
      eval(letbody)((f, Closure(f, x, fbody, env)) :: env)

    case Call(Var(f), earg) =>
      firstorder.lookup(env)(f) match {
        case c @ Closure(f, p, fbody, fenv) =>
          scope match {
            case Static => eval(fbody)((p, Num(eval(earg)(env))) :: (f, c) :: fenv)
            case Dynamic => eval(fbody)((p, Num(eval(earg)(env))) :: env)
          }
        case _ => sys.error("eval Call: not a function")
      }

    case Call(_, _) => sys.error("eval Call: not first-order function")
  }

  def evalStaticScope(e: Expr) = eval(e)(firstorder.emptyEnv)(Static)

  def evalDynamicScope(e: Expr) = eval(e)(firstorder.emptyEnv)(Dynamic)

  val g = LetFun("g", "y", Prim("+", Var("x"), Var("y")), Call(Var("g"), Prim("*", CstI(2), Var("x"))))
  val f = LetFun("f", "x", g, Call(Var("f"), CstI(7)))
  assert(evalStaticScope(f) == 21)

  /**
    * let y = 11 in
    *   let f x = x + y in
    *     let y = 22 in
    *       f 3
    *     end
    *   end
    * end
    */
  val s1 =
    Let("y", CstI(11),
      LetFun("f", "x", Prim("+", Var("x"), Var("y")),
        Let("y", CstI(22),
          Call(Var("f"), CstI(3))
        )
      )
    )
  assert(evalStaticScope(s1) == 14, s"static scope: y is not 11 in $s1")
  assert(evalDynamicScope(s1) == 25, s"dynamic scope: y is not 22 in $s1")

  /**
    * let fact x = if x = 1 then 1 else x * fact(x-1) in fact 3
    */
  val fact = LetFun(
    "fact",
    "x",
    If(Prim("=", Var("x"), CstI(1)), CstI(1), Prim("*", Var("x"), Call(Var("fact"), Prim("-", Var("x"), CstI(1))))),
    Call(Var("fact"), CstI(3)))

  assert(evalStaticScope(fact) == 6, s"fact(3): (3 * 2 * 1) != 6")
}