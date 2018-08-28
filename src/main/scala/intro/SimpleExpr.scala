package intro
import intro.Expr.Env

object Expr {
  type Env = Map[String, Int]

  /**
    * Examples of expressions:
    * 17    as new CstI(17)
    * 3 - a as new Prim("-", new CstI(3), new Var("a"))
    */
}

abstract class Expr {
  import Expr.Env
  def eval(env: Env): Int
}

class CstI(value: Int) extends Expr {
  override def eval(env: Env): Int = value
}
class Var(symbol: String) extends Expr {
  override def eval(env: Env): Int = env.get(symbol).getOrElse(sys.error(symbol + " not found"))
}
class Prim(op: String, left: Expr, right: Expr) extends Expr {
  override def eval(env: Env): Int = {
    if (op == "+") {
      left.eval(env) + right.eval(env)
    } else if (op == "*") {
      left.eval(env) * right.eval(env)
    } else if (op == "-") {
      left.eval(env) - right.eval(env)
    } else {
      sys.error("unknown primitive")
    }
  }
}