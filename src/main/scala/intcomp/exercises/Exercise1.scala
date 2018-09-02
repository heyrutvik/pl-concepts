package intcomp.exercises

import intro.Intro2.{Env, lookup}

object Exercise1 {

  /**
    * add multiple sequential let-bindings in `Expr` from `Intcomp1`
    */
  sealed trait Expr
  case class CstI(value: Int) extends Expr
  case class Var(symbol: String) extends Expr
  case class Let(bindings: List[Bind], body: Expr) extends Expr
  case class Prim(op: String, left: Expr, right: Expr) extends Expr

  // s = rhs bindings
  case class Bind(s: String, rhs: Expr)

  /**
    * evaluator for multiple let-bindings
    */
  def eval(e: Expr)(env: Env): Int = {

    /**
      * `_env` will create environment from multiple let-bindings
      *
      * it will take `env` at the call time and append more pairs
      * and return it as new environment
      *
      * for each "head" (let-binding) we `eval` it using current `env`
      * and append that "pair" to `env` and recurse for "tail" of list
      */
    def _env(bs: List[Bind])(env: Env): Env = bs match {
      case Nil => env
      case Bind(s, rhs) :: t =>
        _env(t)((s, eval(rhs)(env)) :: env)
    }

    e match {
      case CstI(v) => v
      case Var(s) => lookup(env)(s)
      case Let(bs, body) => eval(body)(_env(bs)(env))                  // eval body using `env` of bindings
      case Prim("+", l, r) => eval(l)(env) + eval(r)(env)
      case Prim("*", l, r) => eval(l)(env) * eval(r)(env)
      case Prim("-", l, r) => eval(l)(env) - eval(r)(env)
      case _ => sys.error("unknown expression")
    }
  }

  val e1 = Let(List(Bind("x", CstI(1)), Bind("y", Prim("+", CstI(1), Var("x")))), Prim("+", Var("x"), Var("y")))
  assert(eval(e1)(Nil) == 3, s"eval ${e1} != 3")
}
