package firstorder.exercises

object Exercise12 extends App {

  /**
    * minimal type checker to demonstrate mutual recursion expression type check
    */

  type Env = firstorder.Env[Typ]

  sealed trait Typ
  case object TypI extends Typ
  case object TypB extends Typ
  case class TypF(pt: Typ, rt: Typ) extends Typ

  sealed trait TyExpr
  case class CstI(v: Int) extends TyExpr
  case class CstB(v: Boolean) extends TyExpr
  case class Var(s: String) extends TyExpr
  case class Prim(op: String, left: TyExpr, right: TyExpr) extends TyExpr
  case class If(cond: TyExpr, thenp: TyExpr, elsep: TyExpr) extends TyExpr
  case class LetFun(fs: List[FunDef], lbody: TyExpr) extends TyExpr
  case class Call(left: TyExpr, right: TyExpr) extends TyExpr

  case class FunDef(f: String, p: String, pt: Typ, fbody: TyExpr, rt: Typ)

  def typ(e: TyExpr)(env: Env): Typ = e match {
    case CstI(i) => TypI
    case CstB(b) => TypB
    case Var(s) => firstorder.lookup(env)(s)
    case Prim(op, l, r) =>
      (op, typ(l)(env), typ(r)(env)) match {
        case ("*", TypI, TypI) => TypI
        case ("+", TypI, TypI) => TypI
        case ("-", TypI, TypI) => TypI
        case ("=", TypI, TypI) => TypB
        case ("<", TypI, TypI) => TypB
        case _ => sys.error(s"$op primitive operation not supported")
      }

    case If(cond, thenp, elsep) =>
      typ(cond)(env) match {
        case TypB =>
          val tt = typ(thenp)(env)
          val et = typ(elsep)(env)
          if (tt == et) tt
          else sys.error("branch type not same")
        case _ => sys.error("If: ")
      }

    case LetFun(fs, body) =>
      val fst = fs.flatMap { case FunDef(f, x, xt, b, rt) =>
        List((x, xt), (f, TypF(xt, rt)))
      }
      fs.map { case FunDef(f, x, xt, b, rt) =>
        if (typ(b)(fst ::: env) == rt) rt
        else sys.error("body and return type doesnt match")
      }
      typ(body)(fst ::: env)

    case Call(Var(f), a) =>
      firstorder.lookup(env)(f) match {
        case TypF(pt, rt) if pt == typ(a)(env) => rt
        case _ => sys.error("parameter type doesnt match")
      }

    case Call(_, _) => sys.error("Call: ")
  }

  /**
    * let isEven x = if x = 0 then true else isOdd x-1
    *     isOdd x = if x = 0 then false else isEven x-1
    * in isEven 3
    */
  val isEven = FunDef("isEven", "x", TypI, If(Prim("=", Var("x"), CstI(0)), CstB(true), Call(Var("isOdd"), Prim("-", Var("x"), CstI(1)))), TypB)
  val isOdd = FunDef("isOdd", "x", TypI, If(Prim("=", Var("x"), CstI(0)), CstB(false), Call(Var("isEven"), Prim("-", Var("x"), CstI(1)))), TypB)
  val mr = LetFun(List(isEven, isOdd), Call(Var("isEven"), CstI(3)))

  println(typ(mr)(firstorder.emptyEnv))
}
