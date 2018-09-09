package firstorder.exercises

object Exercise7 {

  /**
    * type checker for functions with multiple arguments as in `Exercise3`
    */

  type Env = firstorder.Env[Typ]

  sealed trait Typ
  case object TypI extends Typ
  case object TypB extends Typ
  case class TypF(pts: List[Typ], rt: Typ) extends Typ

  sealed trait TyExpr
  case class CstI(v: Int) extends TyExpr
  case class CstB(v: Boolean) extends TyExpr
  case class Var(s: String) extends TyExpr
  case class Let(s: String, rhs: TyExpr, body: TyExpr) extends TyExpr
  case class Prim(op: String, left: TyExpr, right: TyExpr) extends TyExpr
  case class If(cond: TyExpr, thenp: TyExpr, elsep: TyExpr) extends TyExpr
  case class LetFun(f: String, pts: List[(String, Typ)], fbody: TyExpr, rt: Typ, lbody: TyExpr) extends TyExpr
  case class Call(f: TyExpr, as: List[TyExpr]) extends TyExpr

  def typ(e: TyExpr)(env: Env): Typ = e match {
    case CstI(_) => TypI
    case CstB(_) => TypB

    case Var(s) => firstorder.lookup(env)(s)

    case Let(s, rhs, body) =>
      typ(body)((s, typ(rhs)(env)) :: env)

    case Prim(op, l, r) =>
      (op, typ(l)(env), typ(r)(env)) match {
        case ("*", TypI, TypI) => TypI
        case ("+", TypI, TypI) => TypI
        case ("-", TypI, TypI) => TypI
        case ("=", TypI, TypI) => TypB
        case ("+", TypI, TypI) => TypB
        case _ => sys.error(s"$op primitive operation not supoorted")
      }

    case If(cond, thenp, elsep) =>
      if (typ(cond)(env) == TypB) {
        val tpt = typ(thenp)(env)
        val elt = typ(elsep)(env)
        if (tpt == elt) tpt else sys.error("both branches should have same type")
      } else
        sys.error(s"condition must be type boolean")

    case LetFun(f, ps, fbody, rt, lbody) =>
      val pts = ps.map(_._2)
      val ft = TypF(pts, rt)
      val bt = typ(fbody)(ps ::: List((f, ft)) ::: env)
      if (bt == rt) {
        typ(lbody)((f, ft) :: env)
      } else {
        sys.error(s"function $f return type: requires $rt but found $bt")
      }

    case Call(Var(f), as) =>
      val ats = as.map(a => typ(a)(env))
      firstorder.lookup(env)(f) match {
        case TypF(pts, rt) if pts == ats => rt
        case TypF(pts, rt) => sys.error(s"function $f param type: requires $pts but found $ats")
        case _ => sys.error("Call: ")
      }

    case Call(_, _) =>
      sys.error("Call: ")
  }

  /**
    * let sum (x: int) (y: int) = (x + y): int in f 2 3 end
    */
  val sum = LetFun("f", List(("x", TypI), ("y", TypI)), Prim("+", Var("x"), Var("y")), TypI, Call(Var("f"), List(CstI(2), CstI(3))))
  assert(typ(sum)(firstorder.emptyEnv) == TypI)

  /**
    * factorial doesn't need second param.. so it's dummy :D
    * let dummyfact x y = if x = 1 then 1 else x * dummyfact(x-1) 1 in fact 3 1
    */
  val dummyfact = LetFun(
    "dummyfact",
    List(("x", TypI), ("y", TypI)),
    If(Prim("=", Var("x"), CstI(1)), CstI(1), Prim("*", Var("x"), Call(Var("dummyfact"), List(Prim("-", Var("x"), CstI(1)), CstI(1))))),
    TypI,
    Call(Var("dummyfact"), List(CstI(3), CstI(1))))
  assert(typ(dummyfact)(firstorder.emptyEnv) == TypI, s"type of $dummyfact != $TypI")
}
