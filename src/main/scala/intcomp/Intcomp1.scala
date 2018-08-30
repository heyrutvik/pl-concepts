package intcomp

import intro.Intro2.{Env, lookup}

object Intcomp1 {

  /**
    * introducing let binding in "abstract syntax"
    *
    * static scope:
    *   of the variable is area of source file it's visible.
    * bound variable:
    *   if it's occurred in expression within scope.
    * free variable:
    *   if it's not bound
    */
  sealed trait Expr
  case class CstI(value: Int) extends Expr
  case class Var(symbol: String) extends Expr
  case class Let(symbol: String, rhs: Expr, body: Expr) extends Expr              // added
  case class Prim(op: String, left: Expr, right: Expr) extends Expr

  // eval with lookup for variable
  def eval(e: Expr)(env: Env): Int = e match {
    case CstI(v) => v
    case Var(symbol) => lookup(env)(symbol)
    case Let(s, rhs, body) =>                                                     // added
      // evaluate `rhs` using original `env`
      val v1 = eval(rhs)(env)
      // create new `env` by prepending `s` and `v1` in original `env`
      val env1 = (s, v1) :: env
      // evaluate `body` using new `env`
      eval(body)(env1)
    case Prim("+", e1, e2) => eval(e1)(env) + eval(e2)(env)
    case Prim("*", e1, e2) => eval(e1)(env) * eval(e2)(env)
    case Prim("-", e1, e2) => eval(e1)(env) - eval(e2)(env)
    case _ => sys.error("unknown primitive")
  }

  val env = List(("x", 10))
  // should become 11 * 2 instead of 10 * 2, so answer is 22
  val e1 = Let("x", CstI(11), Prim("*", Var("x"), CstI(2)))

  assert(eval(e1)(env) == 22, s"eval ${e1} != 22 in env = ${env}")


  /**
    * closed expression:
    *   an expression which doesn't have free variable.
    *
    * `closedin` takes an expression and list of bound variables,
    * and becomes true if given expression `e` is closed in `vs`
    */
  def closedin(e: Expr)(vs: List[String]): Boolean = e match {
    case CstI(_) => true
    case Var(s) => vs.contains(s)
    case Let(s, rhs, body) => closedin(rhs)(vs) && closedin(body)(s :: vs)
    case Prim(_, e1, e2) => closedin(e1)(vs) && closedin(e2)(vs)
  }

  def closed1(e: Expr) = closedin(e)(Nil)

  /**
    * set of free variables
    */
  def union(xs: List[String], ys: List[String]): List[String] = xs match {
    case Nil => ys
    case h :: t => if (ys.contains(h)) union(t, ys) else h :: union(t, ys)
  }

  def minus(xs: List[String], ys: List[String]): List[String] = xs match {
    case Nil => Nil
    case h :: t => if (ys.contains(h)) minus(t, ys) else h :: minus(t, ys)
  }

  def freevars(e: Expr): List[String] = e match {
    case CstI(_) => Nil
    case Var(s) => List(s)
    case Let(s, rhs, body) => union(freevars(rhs), minus(freevars(body), List(s)))
    case Prim(_, e1, e2) => union(freevars(e1), freevars(e2))
  }

  def closed2(e: Expr) = freevars(e) == Nil

  /**
    * substitution: replacing variables with expressions
    */
  type SEnv = List[(String, Expr)]

  def lookOrSelf(env: SEnv)(s: String): Expr = env match {
    case Nil => Var(s)
    case (s1, expr1) :: xs => if (s == s1) expr1 else lookOrSelf(xs)(s)
  }

  def remove(env: SEnv)(s: String): SEnv = env match {
    case Nil => Nil
    case (s1, expr1) :: xs => if (s == s1) xs else (s1, expr1) :: remove(xs)(s)
  }

  def nsubst(e: Expr)(env: SEnv): Expr = e match {
    case CstI(_) => e
    case Var(s) => lookOrSelf(env)(s)
    case Let(s, rhs, body) =>
      Let(s, nsubst(rhs)(env), nsubst(body)(remove(env)(s)))
    case Prim(op, e1, e2) => Prim(op, nsubst(e1)(env), nsubst(e2)(env))
  }

  // [x = 10; y = 1]
  val senv = List(("x", CstI(10)), ("y", CstI(2)))
  // let x = x in x * y where x = 10 and y = 2 so 10 * 2 -> 20
  val se2 = Let("x", Var("x"), Prim("*", Var("x"), Var("y")))
  assert(eval(nsubst(se2)(senv))(env) == 20, s"eval ${se2} != 20 in env = ${env} and senv = ${senv}")


  /**
    * capture-avoiding substitution
    */
  val newVar: String => String = {
    var index: Int = 0
    s: String => {
      index += 1
      s + index
    }
  }

  def subst(e: Expr)(env: SEnv): Expr = e match {
    case CstI(_) => e
    case Var(s) => lookOrSelf(env)(s)
    case Let(s, rhs, body) =>
      val nVar = newVar(s)
      val nEnv = (s, Var(nVar)) :: remove(env)(s)
      Let(nVar, subst(rhs)(env), subst(body)(nEnv))
    case Prim(op, e1, e2) => Prim(op, subst(e1)(env), subst(e2)(env))
  }

  val e9 = Let("z", CstI(22), Prim("+", Var("y"), Var("z")))
  assert(subst(e9)(List(("y", Var("z")))) == Let("z1",CstI(22),Prim("+",Var("z"),Var("z1"))),
    s"subst ${subst(e9)(List(("y", Var("z"))))} != Let(z1,CstI(22),Prim(+,Var(z),Var(z1)))")
}
