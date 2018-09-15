package polymorphic

import higherorder._
import firstorder.{Env, lookup, union, unique}

/**
  * type variable is either "just" a name or link to some other type
  */
sealed trait TypVarKind

case class NoLink(tvn: String) extends TypVarKind                    // `tvn` type variable name
case class LinkTo(t: Typ) extends TypVarKind                         // link to type `t`

// mutable data
case class TypVar(var tvk: TypVarKind, var level: Int)               // type variable kind and binding level

/**
  * possible types
  */
sealed trait Typ

case object TypI extends Typ                                         // integer type
case object TypB extends Typ                                         // boolean type
case class TypF(pt: Typ, rt: Typ) extends Typ                        // function type, could be polymorphic
case class TypV(tv: TypVar) extends Typ                              // type variable

case class TypScheme(tvs: List[TypVar], t: Typ)                      // type scheme

object TypeInference {

  import Helper._

  // type environment
  type TEnv = Env[TypScheme]

  // side-effect function for `TypVar`
  def setTvKind(tv: TypVar, tvk: TypVarKind): Unit = tv.tvk = tvk

  def setTvLevel(tv: TypVar, lvl: Int): Unit = tv.level = lvl

  // unification auxiliary functions

  /**
    * FIND(α) <=> find the canonical representation of class in which α belongs
    *
    * @param t type
    * @return canonical type representation
    */
  def normType(t: Typ): Typ = t match {
    case TypV(tv@TypVar(LinkTo(t1), _)) =>
      val t2 = normType(t1)
      setTvKind(tv, LinkTo(t2))
      t2
    case _ => t
  }

  /**
    * UNION(α, β) <=> make α equal to β
    *
    * link α to the equivalence class of β
    *
    * @param tv type variable
    * @param t  type
    */
  def linkVarToType(tv: TypVar, t: Typ): Unit = {

    /**
      * @param tv  type variable
      * @param tvs list of type variable
      */
    def occurCheck(tv: TypVar, tvs: List[TypVar]): Unit = {
      if (tvs.contains(tv)) sys.error("type error: circularity")
      else ()
    }

    /**
      * traverse the list and set the level of each type variable
      * to the lowest.
      *
      * @param lvl0 binding level of another type variable
      * @param tvs  list of type variable
      */
    def pruneLevel(lvl0: Int, tvs: List[TypVar]) = {
      tvs foreach { case tv@TypVar(_, lvl1) =>
        setTvLevel(tv, lvl0 min lvl1)
      }
    }

    val TypVar(_, lvl) = tv
    val fvs = freeTypeVars(t)                                        // free variables in type t
    occurCheck(tv, fvs)                                              // fvs must not contain tv to avoid circulation
    pruneLevel(lvl, fvs)                                             // set level to the lowest
    setTvKind(tv, LinkTo(t))                                         // add tv to t "class"
  }

  // find type variables
  def freeTypeVars(t: Typ): List[TypVar] = {
    normType(t) match {
      case TypI => Nil
      case TypB => Nil
      case TypV(tv) => List(tv)
      case TypF(pt, rt) =>
        union(freeTypeVars(pt), freeTypeVars(rt))
    }
  }

  /**
    * unification algorithm
    *
    * @param t1
    * @param t2
    */
  def unify(t1: Typ, t2: Typ): Unit = {
    val _t1 = normType(t1)                                           // find canonical form of type t1
    val _t2 = normType(t2)                                           // find canonical form of type t2
    (_t1, _t2) match {                                               // match over both
      case (TypI, TypI) => ()

      case (TypB, TypB) => ()

      case (TypF(p1, r1), TypF(p2, r2)) =>                           // unification of two function type is similar
        unify(p1, p2); unify(r1, r2)                                 // to unification of param and return type

      case (TypV(a), TypV(b)) =>
        val TypVar(_, alvl) = a
        val TypVar(_, blvl) = b
        if (a == b) ()
        else if (alvl < blvl) linkVarToType(a, _t2)                  // link type variable to type
        else linkVarToType(b, _t1)                                   // lower level will point to higher level

      case (TypV(a), _) =>
        linkVarToType(a, _t2)

      case (_, TypV(b)) =>
        linkVarToType(b, _t1)

      case (TypI, t) =>
        sys.error(s"type error: int and ${showType(t)}")

      case (TypB, t) =>
        sys.error(s"type error: int and ${showType(t)}")

      case (TypF(_, _), t) =>
        sys.error(s"type error: int and ${showType(t)}")
    }
  }

  // type inference algorithm and its auxiliary functions

  def specialize(lvl: Int, ts: TypScheme): Typ = {

    def copyType(subst: List[(TypVar, TypV)], t: Typ): Typ = t match {
      case TypI => TypI
      case TypB => TypB
      case TypF(pt, rt) => TypF(copyType(subst, pt), copyType(subst, rt))
      case TypV(tv) => {
        def loop(subst1: List[(TypVar, TypV)]): Typ = subst1 match {
          case Nil =>
            tv match {
              case TypVar(NoLink(_), _) => t
              case TypVar(LinkTo(t), _) => copyType(subst, t)
            }
          case (tv1, t1) :: r =>
            if (tv1 == tv) t1 else loop(r)
        }

        loop(subst)
      }
    }

    val TypScheme(tvs, t) = ts
    tvs match {
      case Nil => t
      case _ =>
        val subst = tvs.map(tv => (tv, TypV(newTypeVar(lvl))))
        copyType(subst, t)
    }
  }

  def generalize(lvl: Int, t: Typ): TypScheme = {
    TypScheme(unique(freeTypeVars(t).filter(tv => tv.level > lvl)), t)
  }

  /**
    * create new type variable at the `lvl` level
    */
  var tvn = 0

  def newTypeVar(lvl: Int): TypVar = {
    def mkName(i: Int, res: List[Char]): List[Char] = {
      if (i < 26) (97 + i).toChar :: res
      else mkName(i / 26 - 1, (97 + i % 26).toChar :: res)
    }

    def intToName(i: Int) = s"'${mkName(i, Nil).mkString}"

    val tv = TypVar(NoLink(intToName(tvn)), lvl)
    tvn += 1
    tv
  }

  def typ(e: Expr, lvl: Int = 0, env: TEnv = Nil): Typ = e match {
    case CstI(_) => TypI                                             // constant integer then type is int

    case CstB(_) => TypB                                             // constant boolean then type is bool

    case Var(s) => specialize(lvl, lookup(env)(s))

    case Prim(op, l, r) =>
      val t1 = typ(l, lvl, env)
      val t2 = typ(r, lvl, env)
      op match {
        case "*" => unify(TypI, t1); unify(TypI, t1); TypI
        case "+" => unify(TypI, t1); unify(TypI, t1); TypI
        case "-" => unify(TypI, t1); unify(TypI, t1); TypI
        case "=" => unify(t1, t2); TypB
        case "<" => unify(TypI, t1); unify(TypI, t1); TypB
        case _ => sys.error(s"unknown primitive $op")
      }

    case Let(s, rhs, body) =>
      val rhst = typ(rhs, lvl + 1, env)
      val letEnv = (s, generalize(lvl, rhst)) :: env
      typ(body, lvl, letEnv)

    case If(cond, thenp, elsep) =>
      val t2 = typ(thenp, lvl, env)
      val t3 = typ(elsep, lvl, env)
      unify(TypB, typ(cond, lvl, env))
      unify(t2, t3)
      t2

    case LetFun(f, p, fbody, lbody) =>
      val ft = TypV(newTypeVar(lvl + 1))
      val pt = TypV(newTypeVar(lvl + 1))
      val fbodyEnv = (p, TypScheme(Nil, pt)) :: (f, TypScheme(Nil, ft)) :: env
      val rt = typ(fbody, lvl, fbodyEnv)
      unify(ft, TypF(pt, rt))
      val lbodyEnv = (f, generalize(lvl, ft)) :: env
      typ(lbody, lvl, lbodyEnv)

    case Call(eFun, eArg) =>
      val ft = typ(eFun, lvl, env)
      val pt = typ(eArg, lvl, env)
      val rt = TypV(newTypeVar(lvl))
      unify(ft, TypF(pt, rt))
      rt
  }

  def typinf(e: Expr) = typ(e)
}

object Helper {

  def showType(t: Typ): String = {
    (TypeInference.normType(t)) match {
      case TypI => "int"
      case TypB => "bool"
      case TypF(p, r) => s"(${showType(p)} -> ${showType(r)})"
      case TypV(TypVar(NoLink(tvn), _)) => tvn
      case TypV(_) => sys.error(s"showType impossible")
    }
  }
}

object Demo extends App {

  import TypeInference._
  import Helper._

  val i = TypI
  val b = TypB

  val fii = TypF(i, i)
  val fib = TypF(i, b)
  val fbb = TypF(b, b)

  val E = TypV(TypVar(NoLink("E"), 0))

  assert(showType(E) == "E", "not E")
  unify(TypI, E)
  assert(showType(E) == "int", "not int")

  val A = TypV(TypVar(NoLink("A"), 0))
  val B = TypV(TypVar(LinkTo(A), 0))

  val C = TypV(TypVar(LinkTo(B), 0))
  val D = TypV(TypVar(NoLink("D"), 0))

  val fAD = TypF(A, D)
  val fBD = TypF(B, D)
  val fDD = TypF(D, D)

  assert(showType(fBD) == "(A -> D)", "not (A -> D)")
  unify(fib, fBD)
  assert(showType(fBD) == "(int -> bool)", "not (int -> bool)")

  /**
    * if given two type variable with different level
    * then lower level get unified to higher level
    *
    * here F got points to G after unification
    * so canonical type of F is G.
    */
  val F = TypV(TypVar(NoLink("F"), 1))
  val G = TypV(TypVar(NoLink("G"), 4))
  assert(showType(F) == "F")
  assert(showType(G) == "G")
  unify(F, G)
  assert(showType(F) == "G")
  assert(showType(G) == "G")

  // let f x = false in f 1 end
  val f = LetFun("f", "x", CstB(false), Call(Var("f"), CstI(1)))
  assert(showType(typ(f)) == "bool", s"type error: in function f required bool")

  /**
    * let twice f = let g x = f(f(x)) in g end
    * in let mul3 z = z*3 in twice mul3 2 end end
    */
  val twice =
    LetFun("twice", "f",
      LetFun("g", "x", Call(Var("f"), Call(Var("f"), Var("x"))), Var("g")),
      LetFun("mul3", "z", Prim("*", Var("z"), CstI(3)),
        Call(Call(Var("twice"), Var("mul3")), CstI(2))))
  assert(showType(typ(twice)) == "int", s"type error: in function twice required int")
}