package intro

object Exercises {

  import Intro2.{ Expr => I2Expr, Env => I2Env, CstI => I2CstI, Var => I2Var, Prim => I2Prim, lookup}

  // Exercise 1.1

  /**
    * (1) extend `eval` from `Intro2` with primitives `max`, `min` and `==`
    */
  def eval(e: I2Expr)(env: I2Env): Int = e match {
    case I2CstI(v) => v
    case I2Var(symbol) => lookup(env)(symbol)
    case I2Prim("+", e1, e2) => eval(e1)(env) + eval(e2)(env)
    case I2Prim("*", e1, e2) => eval(e1)(env) * eval(e2)(env)
    case I2Prim("-", e1, e2) => eval(e1)(env) - eval(e2)(env)
    case I2Prim("max", e1, e2) => eval(e1)(env) max eval(e2)(env)
    case I2Prim("min", e1, e2) => eval(e1)(env) min eval(e2)(env)
    case I2Prim("==", e1, e2) => if (eval(e1)(env) == eval(e2)(env)) 1 else 0
    case _ => sys.error("unknown primitive")
  }

  /**
    * (2) write some example expression and evaluate them using new `eval`
    */
  val env: I2Env = List(("a", 1), ("x", 2))
  // 1 == a -> 1
  val e1: I2Expr = I2Prim("==", I2CstI(1), I2Var("a"))
  // (1+2) max (x min 3) -> 3
  val e2: I2Expr = I2Prim("max", I2Prim("+", I2CstI(1), I2CstI(2)), I2Prim("min", I2Var("x"), I2CstI(3)))

  assert(eval(e1)(env) == 1, s"${e1} != 1 in env: [$env]")
  assert(eval(e2)(env) == 3, s"${e2} != 3 in env: [$env]")
}
