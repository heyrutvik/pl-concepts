package firstorder.typedfun

/**
  * `Typ` representation possible types
  */
sealed trait Typ
case object TypI extends Typ                     // integer type
case object TypB extends Typ                     // boolean type
case class TypF(pt: Typ, rt: Typ) extends Typ    // function param type `pt` and return type `rt`

/**
  * same as `fun.Expr` but added explicit types in function definition
  */
sealed trait TyExpr
case class CstI(v: Int) extends TyExpr
case class CstB(v: Boolean) extends TyExpr
case class Var(s: String) extends TyExpr
case class Let(s: String, rhs: TyExpr, body: TyExpr) extends TyExpr
case class Prim(op: String, left: TyExpr, right: TyExpr) extends TyExpr
case class If(cond: TyExpr, thenp: TyExpr, elsep: TyExpr) extends TyExpr
case class LetFun(s: String, p: String, pt: Typ, fbody: TyExpr, rt: Typ, lbody: TyExpr) extends TyExpr   // types added
case class Call(left: TyExpr, rightL: TyExpr) extends TyExpr

object TypedFun {

  type Env = firstorder.Env[Typ]

  /**
    * type checker gives a type of given expression
    */
  def typ(e: TyExpr)(env: Env): Typ = e match {
    case CstI(_) => TypI

    case CstB(_) => TypB

    case Var(x) => firstorder.lookup(env)(x)

    case Prim(op, l, r) =>
      val lt = typ(l)(env)
      val rt = typ(r)(env)
      (op, lt, rt) match {
        case ("*", TypI, TypI) => TypI
        case ("+", TypI, TypI) => TypI
        case ("-", TypI, TypI) => TypI
        case ("=", TypI, TypI) => TypB
        case ("<", TypI, TypI) => TypB
        case _ => sys.error("unknown op or type error")
      }

    case Let(s, rhs, body) =>
      typ(body)((s, typ(rhs)(env)) :: env)

    case If(cond, thenp, elsep) =>
      typ(cond)(env) match {
        case TypB =>
          val thent = typ(thenp)(env)
          val elset = typ(elsep)(env)
          if (thent == elset) thent
          else sys.error("If: branch of condition has different type")
      }

    case LetFun(f, x, pt, fbody, rt, lbody) =>
      val ft = TypF(pt, rt)
      if (typ(fbody)((x, pt) :: (f, ft) :: env) == rt)
        typ(lbody)((f, ft) :: env)
      else
        sys.error(s"LetFun: return type in $f")

    case Call(Var(f), earg) =>
      firstorder.lookup(env)(f) match {
        case TypF(pt, rt) =>
          if (typ(earg)(env) == pt) rt
          else sys.error("wrong argument type")
      }

    case Call(_, _) => sys.error("Call: unknown function")
  }

  val t1 = LetFun("f", "x", TypI, Prim("+", Var("x"), Var("x")), TypI, Call(Var("f"), CstI(2)))
  assert(typ(t1)(firstorder.emptyEnv) == TypI, s"type of $t1 != $TypI")

  /**
    * let fact x = if x = 1 then 1 else x * fact(x-1) in fact 3
    */
  val fact = LetFun(
    "fact",
    "x",
    TypI,
    If(Prim("=", Var("x"), CstI(1)), CstI(1), Prim("*", Var("x"), Call(Var("fact"), Prim("-", Var("x"), CstI(1))))),
    TypI,
    Call(Var("fact"), CstI(3)))
  assert(typ(fact)(firstorder.emptyEnv) == TypI, s"type of $fact != $TypI")
}