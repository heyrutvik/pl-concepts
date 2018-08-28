package intro.exercises

import intro.Intro2._

object Exercise1 {

  /**
    * (1) extend `eval` from `Intro2` with primitives `max`, `min` and `==`
    */
  def eval1(e: Expr)(env: Env): Int = e match {
    case CstI(v) => v
    case Var(symbol) => lookup(env)(symbol)
    case Prim("+", e1, e2) => eval1(e1)(env) + eval1(e2)(env)
    case Prim("*", e1, e2) => eval1(e1)(env) * eval1(e2)(env)
    case Prim("-", e1, e2) => eval1(e1)(env) - eval1(e2)(env)
    case Prim("max", e1, e2) => eval1(e1)(env) max eval1(e2)(env)
    case Prim("min", e1, e2) => eval1(e1)(env) min eval1(e2)(env)
    case Prim("==", e1, e2) => if (eval1(e1)(env) == eval1(e2)(env)) 1 else 0
    case _ => sys.error("unknown primitive")
  }

  /**
    * (2) write some example expression and evaluate them using new `eval`
    */
  val env: Env = List(("a", 1), ("x", 2))
  // 1 == a -> 1
  val e1: Expr = Prim("==", CstI(1), Var("a"))
  // (1+2) max (x min 3) -> 3
  val e2: Expr = Prim("max", Prim("+", CstI(1), CstI(2)), Prim("min", Var("x"), CstI(3)))

  assert(eval1(e1)(env) == 1, s"eval1 ${e1} != 1 in env: [$env]")
  assert(eval1(e2)(env) == 3, s"eval1 ${e2} != 3 in env: [$env]")

  /**
    * (3) rewrite `eval` which evaluates primitive argument first and then matches
    */
  def eval3(e: Expr)(env: Env): Int = e match {
    case CstI(v) => v
    case Var(symbol) => lookup(env)(symbol)
    case Prim(op, e1, e2) =>
      val i1 = eval3(e1)(env)
      val i2 = eval3(e2)(env)
      op match {
        case "+" => i1 + i2
        case "*" => i1 * i2
        case "-" => i1 - i2
        case "max" => i1 max i2
        case "min" => i1 min i2
        case "==" => if (i1 == i2) 1 else 0
      }
    case _ => sys.error("unknown primitive")
  }

  assert(eval3(e1)(env) == 1, s"eval3 ${e1} != 1 in env: [$env]")
  assert(eval3(e2)(env) == 3, s"eval3 ${e2} != 3 in env: [$env]")

  /**
    * (4) extend language with conditional construct
    * (5) extend `eval` for conditional expression
    *     if e1 is non-zero then evaluate e2 else evaluate e3
    */
  case class If(e1: Expr, e2: Expr, e3: Expr) extends Expr

  def eval5(e: Expr)(env: Env): Int = e match {
    case CstI(v) => v
    case Var(symbol) => lookup(env)(symbol)
    case Prim(op, e1, e2) =>
      val i1 = eval5(e1)(env)
      val i2 = eval5(e2)(env)
      op match {
        case "+" => i1 + i2
        case "*" => i1 * i2
        case "-" => i1 - i2
        case "max" => i1 max i2
        case "min" => i1 min i2
        case "==" => if (i1 == i2) 1 else 0
      }
    case If(e1, e2, e3) =>
      if (eval5(e1)(env) != 0) eval5(e2)(env) else eval5(e3)(env)
    case _ => sys.error("unknown primitive")
  }

  // if 3 min 2 then a else x
  val e3 = If(Prim("min", CstI(2), CstI(3)), Var("a"), Var("x"))

  assert(eval5(e3)(env) == 1, s"eval5 ${e3} != 1 in env: [$env]")
}
