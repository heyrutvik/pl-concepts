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


  /**
    * Integer Addresses instead of Names
    *
    * "abstract syntax" for target expression `t`
    */
  sealed trait TExpr
  case class TCstI(value: Int) extends TExpr
  case class TVar(index: Int) extends TExpr
  case class TLet(rhs: TExpr, body: TExpr) extends TExpr
  case class TPrim(op: String, left: TExpr, right: TExpr) extends TExpr

  /**
    * `tcomp` is a compiler which generates the target expression
    * from the simple expression using a list of free variables it contains
    */
  def tcomp(e: Expr)(vs: List[String]): TExpr = e match {
    case CstI(v) => TCstI(v)
    case Var(s) => TVar(vs.indexOf(s))
    case Let(s, rhs, body) => TLet(tcomp(rhs)(vs), tcomp(body)(s :: vs))
    case Prim(op, left, right) => TPrim(op, tcomp(left)(vs), tcomp(right)(vs))
  }

  /**
    * `teval` is evaluator which evaluates the target expression
    */
  def teval(te: TExpr)(is: List[Int]): Int = te match {
    case TCstI(v) => v
    case TVar(i) => is(i)
    case TLet(rhs, body) =>
      val v1 = teval(rhs)(is)
      val is1 = v1 :: is
      teval(body)(is1)
    case TPrim("+", left, right) => teval(left)(is) + teval(right)(is)
    case TPrim("*", left, right) => teval(left)(is) * teval(right)(is)
    case TPrim("-", left, right) => teval(left)(is) - teval(right)(is)
    case _ => sys.error("unknown target expression")
  }

  /**
    * note:
    *
    * notice that interpreter `eval` had a list of (String, Int) as an environment
    * which is list of variable name and corresponding value.
    *
    * but,
    * two-stage compiled execution has two phase, compile-time `tcomp` and run-time `teval`
    * `tcomp` has list of variable as an environment
    * `teval` has list of value as an environment
    *
    * relation:
    * teval (tcomp e []) [] == eval e []
    *
    * sidenote:
    * we can define `eval1` for closed expression using `tcomp` and `teval` for target expression
    * as follows,
    */

  // if given `e` is closed expression then compile and evaluate using `tcomp` and `teval` respectively
  def eval1(e: Expr) = if (closed1(e)) teval(tcomp(e)(Nil))(Nil) else sys.error(s"expression is not closed: $e")

  val e2 = {
    Let("x", CstI(10),
      Let("y", CstI(20),
        Prim("+", Var("x"), Var("y"))))
  }

  assert(eval1(e2) == 30, s"(teval . tcomp)($e2) != 30")


  /**
    * Stack Machines for Expression Evaluation
    */
  sealed trait RInstr
  case class RCstI(value: Int) extends RInstr
  case object RAdd extends RInstr
  case object RMul extends RInstr
  case object RSub extends RInstr
  case object RDup extends RInstr
  case object RSwap extends RInstr

  /**
    * evaluator for reverse polish expression
    * state of evaluator depicted as combination of `c` and `s`;
    * `c` is control - list of instruction yet to evaluate
    * `s` is stack for values - intermediate or final result
    */
  def reval(c: List[RInstr])(s: List[Int]): Int = (c, s) match {
    case (Nil, v :: _) => v
    case (Nil, Nil) => sys.error("no result on stack")
    case (RCstI(v) :: tail, s) => reval(tail)(v :: s)                  // constant: push on stack
    case (RAdd :: tail, v1 :: v2 :: s) => reval(tail)(v1 + v2 :: s)    // add: push sum of top two popped value
    case (RMul :: tail, v1 :: v2 :: s) => reval(tail)(v1 * v2 :: s)    // mul: push multiplication of top two popped value
    case (RSub :: tail, v1 :: v2 :: s) => reval(tail)(v1 - v2 :: s)    // sub: push subtract of top two popped value
    case (RDup :: tail, v1 :: s) => reval(tail)(v1 :: v1 :: s)         // duplicate: push same value as top
    case (RSwap :: tail, v1 :: v2 :: s) => reval(tail)(v2 :: v1 :: s)  // swap: push swap top two popped value
    case _ => sys.error("unknown expression")
  }

  val re1 = List(RCstI(1), RCstI(1), RAdd, RCstI(2), RMul)
  assert(reval(re1)(Nil) == 4, s"rinstreval ${re1} != 4")
}
