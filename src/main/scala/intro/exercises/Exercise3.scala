package intro.exercises

import intro.exercises.Exercise2._

object Exercise3 extends App {

  /**
    * smart `fmt` function
    * remove unnecessary parenthesis and take account of operator precedence
    */
  def fmt(e: AExpr): String = {
    import Precedence._
    def _fmt(pre: Int)(e: AExpr): String = e match {
      case CstI(value) => value.toString
      case Var(symbol) => symbol
      case Add(e1, e2) =>
        val s = s"${_fmt(ADD)(e1)} + ${_fmt(ADD)(e2)}"
        if (ADD > pre) s
        else s"($s)"
      case Mul(e1, e2) =>
        val s = s"${_fmt(MUL)(e1)} * ${_fmt(MUL)(e2)}"
        if (MUL > pre) s
        else s"($s)"
      case Sub(e1, e2) =>
        val s = s"${_fmt(SUB-1)(e1)} - ${_fmt(SUB)(e2)}"
        if (SUB > pre) s
        else s"($s)"
    }
    _fmt(NONE)(e)
  }

  val ae1 = Sub(Var("a"), Sub(Var("b"), Var("c")))
  assert(fmt(ae1) == "a - (b - c)", s"fmt ${fmt(ae1)} != a - (b - c)")
  val ae2 = Sub(Sub(Var("a"), Var("b")), Var("c"))
  assert(fmt(ae2) == "a - b - c", s"fmt ${fmt(ae2)} != a - b - c")

  object Precedence {
    val NONE = 0
    val CONST = 1
    val VAR = 1
    val ADD = 2
    val SUB = 2
    val MUL = 3
  }
}
