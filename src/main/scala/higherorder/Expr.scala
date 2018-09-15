package higherorder

/**
  * same as `firstorder.fun.Expr` except the logic in `Call` case
  */

// NOTE: `sealed` removed from `Expr` declaration to extend it in `polymorphic.exercises.Exercise2`
trait Expr
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

  sealed trait Value
  case class Num(i: Int) extends Value
  case class Closure(s: String, p: String, fbody: Expr, fenv: Env) extends Value

  // evaluating the functional language
  private def eval(e: Expr)(env: Env): Value = e match {
    case CstI(i) => Num(i)

    case CstB(b) => Num(if (b) 1 else 0)

    case Var(x) =>
      firstorder.lookup(env)(x)

    case Prim(op, l, r) =>
      val Num(l1) = eval(l)(env)
      val Num(r1) = eval(r)(env)
      val ret = op match {
        case "*" => l1 * r1
        case "+" => l1 + r1
        case "-" => l1 - r1
        case "=" => if (l1 == r1) 1 else 0
        case "<" => if (l1 < r1) 1 else 0
        case _ => sys.error(s"unknown primitive $op")
      }
      Num(ret)

    case Let(s, rhs, body) =>
      val v = eval(rhs)(env)
      eval(body)((s, v) :: env)

    // new cases added here for first-order language
    case If(cond, thenp, elsep) =>
      if (eval(cond)(env) != Num(0)) eval(thenp)(env) else eval(elsep)(env)

    case LetFun(f, x, fbody, letbody) =>
      eval(letbody)((f, Closure(f, x, fbody, env)) :: env)

    /**
      * to make the higher-order expression interpreter
      * only changes we made here is in the `Call`
      *
      * in first-order, we only expect `Var` in `left` of `Call` data constructor
      * now it could be anything which evaluates to function
      */

    case Call(eFun, eArg) =>
      val fc = eval(eFun)(env)
      fc match {
        case Closure(f, x, fb, fenv) =>
          val xv = eval(eArg)(env)
          eval(fb)((x, xv) :: (f, fc) :: fenv)
        case _ => sys.error("eval: call not a function")
      }
  }

  val g = LetFun("g", "y", Prim("+", Var("x"), Var("y")), Call(Var("g"), Prim("*", CstI(2), Var("x"))))
  val f = LetFun("f", "x", g, Call(Var("f"), CstI(7)))
  assert(eval(f)(firstorder.emptyEnv) == Num(21))

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
  assert(eval(s1)(firstorder.emptyEnv) == Num(14), s"static scope: y is not 11 in $s1")

  /**
    * let fact x = if x = 1 then 1 else x * fact(x-1) in fact 3
    */
  val fact = LetFun(
    "fact",
    "x",
    If(Prim("=", Var("x"), CstI(1)), CstI(1), Prim("*", Var("x"), Call(Var("fact"), Prim("-", Var("x"), CstI(1))))),
    Call(Var("fact"), CstI(3)))

  assert(eval(fact)(firstorder.emptyEnv) == Num(6), s"fact(3): (3 * 2 * 1) != 6")
}