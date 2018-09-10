package firstorder.exercises

object Exercise9 {

  /**
    * type checker for list, tuple and match expression
    */

  type Env = firstorder.Env[Typ]

  sealed trait Typ
  case object TypI extends Typ                                       // integer type
  case object TypB extends Typ                                       // boolean type
  case class TypT(ts: List[Typ]) extends Typ                         // tuple type
  case class TypL(t: Typ) extends Typ                                // list type
  case class TypF(pt: List[Typ], rt: Typ) extends Typ                // function type

  sealed trait TyExpr
  case class CstI(v: Int) extends TyExpr
  case class CstB(v: Boolean) extends TyExpr
  case class Var(s: String) extends TyExpr
  // let s: int = 1
  // let ls: [int] = ConC(1, CstN)
  case class Let(s: (String, Typ), rhs: TyExpr, body: TyExpr) extends TyExpr
  case class Prim(op: String, left: TyExpr, right: TyExpr) extends TyExpr
  case class If(cond: TyExpr, thenp: TyExpr, elsep: TyExpr) extends TyExpr
  case class Tup(ts: List[TyExpr]) extends TyExpr
  case class CstN(t: Typ) extends TyExpr
  case class ConC(h: TyExpr, t: TyExpr) extends TyExpr
  case class Match(w: TyExpr, ncase: TyExpr, rcase: RCase) extends TyExpr
  case class LetFun(f: String, pts: List[(String, Typ)], fbody: TyExpr, rt: Typ, lbody: TyExpr) extends TyExpr
  case class Call(f: TyExpr, as: List[TyExpr]) extends TyExpr

  case class RCase(h: String, t: String, rhs: TyExpr)

  def typ(e: TyExpr)(env: Env): Typ = e match {
    case CstI(_) => TypI

    case CstB(_) => TypB

    case Var(s) => firstorder.lookup(env)(s)

    case Let((s, t), rhs, body) =>
      val rhst = typ(rhs)(env)
      if (t == rhst) {
        typ(body)((s, t) :: env)
      } else sys.error(s"type error at $rhs: required $t found $rhst")

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
          else sys.error("if branch types should be same")
        case _ => sys.error("If: ")
      }

    case Tup(ts) => TypT(ts.map(t => typ(t)(env)))

    case CstN(t) => TypL(t)

    case ConC(h, t) =>
      val tt = typ(t)(env)
      typ(h)(env) match {
        case TypI if tt == TypL(TypI) => tt
        case TypB if tt == TypL(TypB) => tt
        case TypT(xs) if tt == TypT(xs) => tt
        case _ => sys.error("list type error")
      }

    case Match(w, ncase, RCase(h, t, rcase)) =>
      val ncaset = typ(ncase)(env)
      typ(w)(env) match {
        case lt @ TypL(ty) =>
          if (ty == ncaset)
            typ(rcase)((h, ty) :: (t, lt) :: env)
          else sys.error("base case type mismatch")
        case _ => sys.error(s"$w must have list type to match")
      }

    case LetFun(f, pts, fbody, rt, lbody) =>
      val ft = TypF(pts.map(_._2), rt)
      val fbodyt = typ(fbody)(pts ::: List((f, ft)) ::: env)
      if (fbodyt == rt) {
        typ(lbody)((f, ft) :: env)
      } else sys.error(s"$f function return type required $rt found $fbodyt")

    case Call(Var(f), as) =>
      firstorder.lookup(env)(f) match {
        case TypF(pt, rt) =>
          val ast = as.map(a => typ(a)(env))
          if (pt == ast)
            rt
          else sys.error(s"$f function expects $pt found $ast")
        case _ => sys.error(s"$f is not a function")
      }

    case Call(_, _) => sys.error("Call: ")
  }

  // let i: int = 1 in i end
  val i = Let(("i", TypI), CstI(1), Var("i"))
  assert(typ(i)(firstorder.emptyEnv) == TypI)

  // let x = true in if x then 1 else 2 end
  val if1 = Let(("x", TypB), CstB(true), If(Var("x"), CstI(1), CstI(2)))
  assert(typ(if1)(firstorder.emptyEnv) == TypI)

  // let t: (int, bool) = (1, false) in t end
  val t = Let(("t", TypT(List(TypI, TypB))), Tup(List(CstI(1), CstB(false))), Var("t"))
  assert(typ(t)(firstorder.emptyEnv) == TypT(List(TypI, TypB)))

  // let ls: [int] = ConC(1, ConC(2, CstN)) in ls end
  val ls = Let(("ls", TypL(TypI)), ConC(CstI(1), CstN(TypI)), Var("ls"))
  assert(typ(ls)(firstorder.emptyEnv) == TypL(TypI))

  /**
    * let sum l: [int] = match l with
    *   [] -> 0
    *   h :: t -> h + sum(t)
    * in sum ([1,2])
    */
  val l = ConC(CstI(1), ConC(CstI(2), CstN(TypI)))
  val sum =
    LetFun(
      "sum",
      List(("l", TypL(TypI))),
      Match(
        Var("l"),                                                              // match list `l`
        CstI(0),                                                               // []     -> 0
        RCase("h", "t", Prim("+", Var("h"), Call(Var("sum"), List(Var("t"))))) // h :: t -> h + f(t)
      ),
      TypI,
      Call(Var("sum"), List(l))
    )
  assert(typ(sum)(firstorder.emptyEnv) == TypI)
}
