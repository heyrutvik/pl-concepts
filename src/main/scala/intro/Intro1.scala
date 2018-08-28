package intro

/**
  * Expressions without variables
  */

object Intro1 {

  /**
    * "abstract syntax" representation of expression using trait `Expr`
    * integer construct using `CstI` data constructor
    * binary operation using `Prim` data constructor
    */

  trait Expr
  case class CstI(value: Int) extends Expr
  case class Prim(op: String, left: Expr, right: Expr) extends Expr

  /**
    * Examples of expressions:
    * 17    as CstI(17)
    * 3 - 4 as Prim("-", CstI(3), CstI(4))
    */

  /**
    * evaluator for an expression
    */

  def eval(e: Expr): Int = e match {
    case CstI(v) => v
    case Prim("+", e1, e2) => eval(e1) + eval(e2)
    case Prim("*", e1, e2) => eval(e1) * eval(e2)
    case Prim("-", e1, e2) => eval(e1) - eval(e2)
    case _ => sys.error("unknown primitive")
  }

  // eval with cut-off subtraction
  def evalm(e: Expr): Int = e match {
    case CstI(v) => v
    case Prim("+", e1, e2) => evalm(e1) + evalm(e2)
    case Prim("*", e1, e2) => evalm(e1) * evalm(e2)
    case Prim("-", e1, e2) =>
      val res = evalm(e1) - evalm(e2)
      if (res < 0) 0 else res
    case _ => sys.error("unknown primitive")
  }
}
