package polymorphic

import higherorder._
import firstorder.{Env, lookup, union, unique}

/**
  * type variable is either "just" a name or link to some other type
  */
sealed trait TypVarKind
case class NoLink(tvn: String) extends TypVarKind          // `tvn` type variable name
case class LinkTo(t: Typ) extends TypVarKind               // link to type `t`

// mutable data
case class TypVar(var tvk: TypVarKind, var level: Int)     // type variable kind and binding level

/**
  * possible types
  */
sealed trait Typ
case object TypI extends Typ                               // integer type
case object TypB extends Typ                               // boolean type
case class TypF(pt: Typ, rt: Typ) extends Typ              // function type, could be polymorphic
case class TypV(tv: TypVar) extends Typ                    // type variable

case class TypScheme(tvs: List[TypVar], t: Typ)            // type scheme

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
    case TypV(tv @ TypVar(LinkTo(t1), _)) =>
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
    * @param t type
    */
  def linkVarToType(tv: TypVar, t: Typ): Unit = {

    /**
      * @param tv type variable
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
      * @param tvs list of type variable
      */
    def pruneLevel(lvl0: Int, tvs: List[TypVar]) = {
      tvs foreach { case tv @ TypVar(_, lvl1) =>
        setTvLevel(tv, lvl0 min lvl1)
      }
    }

    val TypVar(_, lvl) = tv
    val fvs = freeTypeVars(t)                              // free variables in type t
    occurCheck(tv, fvs)                                    // fvs must not contain tv to avoid circulation
    pruneLevel(lvl, fvs)                                   // set level to the lowest
    setTvKind(tv, LinkTo(t))                               // add tv to t "class"
  }

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
    val _t1 = normType(t1)                                 // find canonical form of type t1
    val _t2 = normType(t2)                                 // find canonical form of type t2
    (_t1, _t2) match {                                     // match over both
      case (TypI, TypI) => ()

      case (TypB, TypB) => ()

      case (TypF(p1, r1), TypF(p2, r2)) =>                 // unification of two function type is similar
        unify(p1, p2); unify(r1, r2)                       // to unification of param and return type

      case (TypV(a), TypV(b)) =>
        val TypVar(_, alvl) = a
        val TypVar(_, blvl) = b
        if (a == b) ()
        else if (alvl < blvl) linkVarToType(a, _t2)
        else linkVarToType(b, _t1)

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

  /**
    * type inference algorithm
    */

  def specialize(lvl: Int, ts: TypScheme): Typ = ???

  def generalize(lvl: Int, t: Typ): TypScheme = ???

  def newTypeVar(lvl: Int): TypVar = ???

  def typ(lvl: Int)(env: TEnv)(e: Expr): Typ = e match {
    case CstI(_) => TypI

    case CstB(_) => TypB

    case Var(s) => specialize(lvl, lookup(env)(s))

    case Prim(op, l, r) =>
      val t1 = typ(lvl)(env)(l)
      val t2 = typ(lvl)(env)(r)
      op match {
        case "*" => unify(TypI, t1); unify(TypI, t1); TypI
        case "+" => unify(TypI, t1); unify(TypI, t1); TypI
        case "-" => unify(TypI, t1); unify(TypI, t1); TypI
        case "=" => unify(t1, t2); TypB
        case "<" => unify(TypI, t1); unify(TypI, t1); TypB
        case _ => sys.error(s"unknown primitive $op")
      }

    case Let(s, rhs, body) =>
      val rhst = typ(lvl+1)(env)(rhs)
      val letEnv = (s, generalize(lvl, rhst)) :: env
      typ(lvl)(letEnv)(body)

    case If(cond, thenp, elsep) =>
      val t2 = typ(lvl)(env)(thenp)
      val t3 = typ(lvl)(env)(elsep)
      unify(TypB, typ(lvl)(env)(cond))
      unify(t2, t3)
      t2

    case LetFun(f, x, fbody, lbody) =>
      val ft = TypV(newTypeVar(lvl+1))
      val xt = TypV(newTypeVar(lvl+1))
      val fbodyEnv = (x, TypScheme(Nil, xt)) :: (f, TypScheme(Nil, ft)) :: env
      val rt = typ(lvl)(fbodyEnv)(fbody)
      unify(ft, TypF(xt, rt))
      val lbodyEnv = (f, generalize(lvl, ft)) :: env
      typ(lvl)(lbodyEnv)(lbody)

    case Lambda(x, fbody) => sys.error(s"lambda not supported yet!")

    case Call(eFun, eArg) =>
      val tf = typ(lvl)(env)(eFun)
      val tx = typ(lvl)(env)(eArg)
      val tr = TypV(newTypeVar(lvl))
      unify(tr, TypF(tx, tr))
      tr
  }
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

  val A = TypV(TypVar(NoLink("A"), 0))
  val B = TypV(TypVar(LinkTo(A), 0))

  val C = TypV(TypVar(LinkTo(B), 0))
  val D = TypV(TypVar(NoLink("D"), 0))

  val fAD = TypF(A, D)
  val fBD = TypF(B, D)
}