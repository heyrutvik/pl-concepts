package intro

/**
  * Expressions with variables
  */

object Intro2 {

  /**
    * same "abstract syntax" representation as `Intro1` with added `Var` construct
    *
    * note: trait is not `sealed` by intention
    *       see Exercise1 (4)
    */

  trait Expr
  case class CstI(value: Int) extends Expr
  case class Var(symbol: String) extends Expr
  case class Prim(op: String, left: Expr, right: Expr) extends Expr

  /**
    * Examples of expressions:
    * 17    as CstI(17)
    * x     as Var("x")
    * 3 + a as Prim("+", CstI(3), Var("a"))
    */

  // environment type
  type Env = List[(String, Int)]

  /**
    * to look up a variable in an environment
    * note: method will return first value associated with symbol
    *       lookup(List(("x", 11), ("x", 22)))("x") == 11
    */
  def lookup(env: Env)(symbol: String): Int = env match {
    case Nil => sys.error(symbol + " not found")
    case (k, v) :: r => if (k == symbol) v else lookup(r)(symbol)
  }

  // eval with lookup for variable
  def eval(e: Expr)(env: Env): Int = e match {
    case CstI(v) => v
    case Var(symbol) => lookup(env)(symbol)
    case Prim("+", e1, e2) => eval(e1)(env) + eval(e2)(env)
    case Prim("*", e1, e2) => eval(e1)(env) * eval(e2)(env)
    case Prim("-", e1, e2) => eval(e1)(env) - eval(e2)(env)
    case _ => sys.error("unknown primitive")
  }
}
