package intro.exercises

object Exercise2 {

  /**
    * (1) alternative datatype aexpr for a representation of arithmetic expressions
    */
  sealed trait AExpr
  case class CstI(value: Int) extends AExpr
  case class Var(symbol: String) extends AExpr
  case class Add(e1: AExpr, e2: AExpr) extends AExpr
  case class Mul(e1: AExpr, e2: AExpr) extends AExpr
  case class Sub(e1: AExpr, e2: AExpr) extends AExpr

  // x * (y + 3)
  val ae1 = Mul(Var("x"), Add(Var("y"), CstI(3)))

  /**
    * (2) write representation of following expressions
    */

  // v - (w + z)
  val ae2 = Sub(Var("v"), Add(Var("w"), Var("z")))
  // 2 * (v - (w + z))
  val ae3 = Mul(CstI(2), Sub(Var("v"), Add(Var("w"), Var("z"))))

  /**
    * (3) format expression as string
    */
  def fmt(e: AExpr): String = e match {
    case CstI(v) => v.toString
    case Var(symbol) => symbol
    case Add(e1, e2) => s"(${fmt(e1)} + ${fmt(e2)})"
    case Mul(e1, e2) => s"(${fmt(e1)} * ${fmt(e2)})"
    case Sub(e1, e2) => s"(${fmt(e1)} - ${fmt(e2)})"
    case _ => sys.error("unknown expression")
  }

  val ae4 = Sub(Var("x"), CstI(34))
  assert(fmt(ae4) == "(x - 34)", s"fmt ${ae4} != (x - 34)")
  assert(fmt(ae3) == "(2 * (v - (w + z)))", s"fmt ${ae4} != (2 * (v - (w + z)))")
  assert(fmt(ae2) == "(v - (w + z))", s"fmt ${ae4} != (v - (w + z))")
  assert(fmt(ae1) == "(x * (y + 3))", s"fmt ${ae4} != (x * (y + 3))")

  /**
    * (4) perform expression simplification
    */
  // TODO some input induces the infinite loop
  def simplify(e: AExpr): AExpr = e match {
    case const @ CstI(_) => const
    case variable @ Var(_) => variable

    case Add(e1, CstI(0)) => simplify(e1)                               // e1 + 0 -> e1
    case Add(CstI(0), e2) => simplify(e2)                               // 0 + e2 -> e2
    case Add(CstI(x), CstI(y)) => CstI(x+y)
    case Add(e1, e2) => simplify(Add(simplify(e1), simplify(e2)))       // simplify sub-expressions and whole

    case Mul(e1, CstI(1)) => simplify(e1)                               // e1 * 1 -> e1
    case Mul(CstI(1), e2) => simplify(e2)                               // 1 * e2 -> e2
    case Mul(_, zero @ CstI(0)) => zero                                 // e * 0 -> 0
    case Mul(zero @ CstI(0), _) => zero                                 // 0 * e -> 0
    case Mul(CstI(x), CstI(y)) => CstI(x*y)
    case Mul(e1, e2) => simplify(Mul(simplify(e1), simplify(e2)))       // simplify sub-expressions and whole

    case Sub(e1, CstI(0)) => simplify(e1)                               // e1 - 0 -> e1
    case Sub(e1, e2) if e1 == e2 => CstI(0)                             // e1 - e2 -> 0 where e1 == e2
    case Sub(CstI(x), CstI(y)) => CstI(x-y)
    case Sub(e1, e2) => simplify(Sub(simplify(e1), simplify(e2)))       // simplify sub-expressions and whole
  }

  // (1 + 0) ∗ (x + 0) -> x
  val ae5 = Mul(Add(CstI(1), CstI(0)), Add(Var("x"), CstI(0)))
  assert(simplify(ae5) == Var("x"), s"simplify ${simplify(ae5)} != x")

  /**
    * (5) perform symbolic differentiation of simple arithmetic
    *     expressions with respect to a single variable
    *
    *     for following reduction rule:
    *
    *     => dc/dx = 0 where c is constant or any other variable than x
    *
    *     => dx/dx = 1
    *
    *     => d(u+v)/dx = du/dx + dv/dx
    *
    *     => d(u*v)/dx = u*(dv/dx) + v*(du/dx)
    */
  def diff(e: AExpr, d: Var): AExpr = e match {
    case CstI(_) => CstI(0)
    case variable @ Var(_) => if (variable == d) CstI(1) else CstI(0)
    case Add(u, v) => Add(diff(u, d), diff(v, d))
    case Mul(u, v) => Add(Mul(u, diff(v, d)), Mul(v, diff(u, d)))
    case _ => sys.error("expression not supported")
  }

  def dx(e: AExpr): AExpr = simplify(diff(e, Var("x")))

  // (x + 3)
  val ae6 = Add(Var("x"), CstI(3))
  // (x * y)
  val ae7 = Mul(Var("x"), Var("y"))
  // ((x * y) * (x + 3))
  val ae8 = Mul(Mul(Var("x"), Var("y")), Add(Var("x"), CstI(3)))

  assert(dx(ae6) == CstI(1), s"dx ${dx(ae6)} != 1")
  assert(dx(ae7) == Var("y"), s"dx ${dx(ae7)} != y")
}
