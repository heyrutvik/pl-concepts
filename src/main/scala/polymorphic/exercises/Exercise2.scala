package polymorphic.exercises

import higherorder.{Expr, CstI, CstB, Var, Let, Prim, If, LetFun, Call}
import firstorder.{lookup, emptyEnv}

/**
  * using `higherorder.Expr` and adding `Fun` case to add lambda expression in abstract syntax
  */

case class Fun(p: String, body: Expr) extends Expr

object Exercise2 {

  // re-implementing evaluator

  type Env = firstorder.Env[Value]

  sealed trait Value
  case class Num(v: Int) extends Value
  case class Closure(f: String, p: String, fbody: Expr, fenv: Env) extends Value
  case class Clos(p: String, fbody: Expr, fenv: Env) extends Value

  def eval(e: Expr)(env: Env): Value = e match {
    case CstI(i) => Num(i)

    case CstB(b) => Num(if (b) 1 else 0)

    case Var(s) => lookup(env)(s)

    case Let(s, rhs, body) =>
      eval(body)((s, eval(rhs)(env)) :: env)

    case Prim(op, l, r) =>
      val Num(l1) = eval(l)(env)
      val Num(r1) = eval(r)(env)
      val ret = op match {
        case "*" => l1 * r1
        case "+" => l1 + r1
        case "-" => l1 - r1
        case "=" => if (l1 == r1) 1 else 0
        case "<" => if (l1 < r1) 1 else 0
        case _ => sys.error(s"premitive operation $op not supported")
      }
      Num(ret)

    case If(cond, thenp, elsep) =>
      val Num(c) = eval(cond)(env)
      if (c != 0) eval(thenp)(env) else eval(elsep)(env)

    case LetFun(f, p, fbody, lbody) =>
      eval(lbody)((f, Closure(f, p, fbody, env)) :: env)             // create closure/env using `fbody` and eval `lbody`

    case Call(l, r) =>
      eval(l)(env) match {                                           // by evaluating `l` get the `Closure` or `Clos`
        case fc @ Closure(f, p, fbody, fenv) =>
          eval(fbody)((f, fc) :: (p, eval(r)(env)) :: fenv)          // add `f` and `p` to env and call eval on `fbody`
        case Clos(p, fbody, fenv) =>
          eval(fbody)((p, eval(r)(env)) :: fenv)                     // if lambda expression then no need to add itself to `env`
        case _ => sys.error("Call: ")
      }

    case Fun(p, body) =>
      Clos(p, body, env)                                             // create `Clos` value for lambda expression
  }

  /**
    * // F#
    * let double f = \x => f (f x) in (double (\x => x + 1))(1) end
    *
    * // Scala
    * val double(f: Int => Int) = { x =>
    *   f(f(x))
    * }
    * val g = double(x => x + 1)
    * g(1)
    */
  val double =
    LetFun(
      "double",
      "f",
      Fun("x", Call(Var("f"), Call(Var("f"), Var("x")))),
      Call(Call(Var("double"), Fun("x", Prim("+", Var("x"), CstI(1)))), CstI(1)))
  assert(eval(double)(emptyEnv) == Num(3))

  // let x = 1 in let f y = x + y in f 1 end end
  val freeFun =
    Let("x",
      CstI(1),
      LetFun("f", "y", Prim("+", Var("y"), Var("y")), Call(Var("f"), CstI(1))))
  assert(eval(freeFun)(emptyEnv) == Num(2))

  // (fun x -> x * 2)(2)
  val directFun = Call(Fun("x", Prim("*", Var("x"), CstI(2))), CstI(2))
  assert(eval(directFun)(emptyEnv) == Num(4))
}
