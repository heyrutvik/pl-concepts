package intro.exercises

object Exercise4 {

  type Env = Map[String, Int]

  /**
    * (1) arithmetic expression using a class hierarchy and composites and implement the string format
    * (3) implement `eval` method
    * (4) algebraic simplifications using `simplify`
    */

  abstract class AExpr {
    def fmt: String
    def eval(env: Env): Int
    def simplify: AExpr
  }
  class CstI(val value: Int) extends AExpr {
    override def fmt: String = value.toString
    override def eval(env: Env): Int = value
    override def simplify: AExpr = new CstI(value)
  }
  class Var(val symbol: String) extends AExpr {
    override def fmt: String = symbol
    override def eval(env: Env): Int = env.get(symbol).getOrElse(sys.error("variable binding not found"))
    override def simplify: AExpr = new Var(symbol)
  }
  abstract class Binop(val e1: AExpr, val e2: AExpr) extends AExpr {}
  class Add(e1: AExpr, e2: AExpr) extends Binop(e1, e2) {
    override def fmt: String = s"(${e1.fmt} + ${e2.fmt})"
    override def eval(env: Env): Int = e1.eval(env) + e2.eval(env)
    override def simplify: AExpr = {
      if (e1.isInstanceOf[CstI] && e2.isInstanceOf[CstI]) {
        if (e1.asInstanceOf[CstI].value == 0) e2.simplify
        else if (e2.asInstanceOf[CstI].value == 0) e1.simplify
        else {
          val v1 = e1.asInstanceOf[CstI].value
          val v2 = e2.asInstanceOf[CstI].value
          new CstI(v1 + v2)
        }
      } else if (e1.isInstanceOf[CstI] && e1.asInstanceOf[CstI].value == 0) {
        e2.simplify
      } else if (e2.isInstanceOf[CstI] && e2.asInstanceOf[CstI].value == 0) {
        e1.simplify
      } else {
        (new Add(e1.simplify, e2.simplify)).simplify
      }
    }
  }
  class Mul(e1: AExpr, e2: AExpr) extends Binop(e1, e2) {
    override def fmt: String = s"(${e1.fmt} * ${e2.fmt})"
    override def eval(env: Env): Int = e1.eval(env) * e2.eval(env)
    override def simplify: AExpr = {
      if (e1.isInstanceOf[CstI] && e2.isInstanceOf[CstI]) {
        if (e1.asInstanceOf[CstI].value == 1) e2.simplify
        else if (e2.asInstanceOf[CstI].value == 1) e1.simplify
        else if (e1.asInstanceOf[CstI].value == 0) new CstI(0)
        else if (e2.asInstanceOf[CstI].value == 0) new CstI(0)
        else {
          val v1 = e1.asInstanceOf[CstI].value
          val v2 = e2.asInstanceOf[CstI].value
          new CstI(v1 * v2)
        }
      } else if (e1.isInstanceOf[CstI] && e1.asInstanceOf[CstI].value == 1) {
        e2.simplify
      } else if (e2.isInstanceOf[CstI] && e2.asInstanceOf[CstI].value == 1) {
        e1.simplify
      } else {
        (new Mul(e1.simplify, e2.simplify)).simplify
      }
    }
  }
  class Sub(e1: AExpr, e2: AExpr) extends Binop(e1, e2) {
    override def fmt: String = s"(${e1.fmt} - ${e2.fmt})"
    override def eval(env: Env): Int = e1.eval(env) - e2.eval(env)
    override def simplify: AExpr = {
      if (e1.isInstanceOf[CstI] && e2.isInstanceOf[CstI]) {
        if (e2.asInstanceOf[CstI].value == 0) e1.simplify
        else if (e1.asInstanceOf[CstI].value == e2.asInstanceOf[CstI].value) new CstI(0)
        else {
          val v1 = e1.asInstanceOf[CstI].value
          val v2 = e2.asInstanceOf[CstI].value
          new CstI(v1 - v2)
        }
      } else {
        (new Sub(e1.simplify, e2.simplify)).simplify
      }
    }
  }

  val ae1: AExpr = new Add(new CstI(17), new Var("z"))
  assert(ae1.fmt == "(17 + z)", s"fmt ${ae1.fmt} != (17 + z)")

  val e1: AExpr = new Add(new CstI(1), new CstI(0))
  assert(e1.simplify.asInstanceOf[CstI].value == 1, s"simplify ${e1.simplify}} != 1")

  // (1 + 0) ∗ (x + 0) -> x
  val ae2: AExpr = new Mul(e1, new Add(new Var("x"), new CstI(0)))
  assert(ae2.simplify.asInstanceOf[Var].symbol == "x", s"simplify ${ae2.simplify}} != x")

  /**
    * (2) more examples
    */

  val ae3: AExpr = new Mul(new CstI(2), new Sub(new Var("v"), new Add(new Var("w"), new Var("z"))))
  assert(ae3.fmt == "(2 * (v - (w + z)))", s"fmt ${ae3.fmt} != (2 * (v - (w + z)))")
}
