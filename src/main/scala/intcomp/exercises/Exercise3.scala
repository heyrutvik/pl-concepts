package intcomp.exercises

import intcomp.Intcomp1.{TCstI, TExpr, TPrim, TVar, TLet, teval}
import intcomp.exercises.Exercise1._

object Exercise3 {

  /**
    * compiler to convert multiple sequential
    * let-binding expression to target expression
    */
  def tcomp(e: Expr)(vs: List[String]): TExpr = {

    /**
      * let-binding compiling logic
      *
      * Nil => compile `body` with accumulated variable bind stack `vs`
      * Bind :: t => compile `rhs` with current `vs`
      *              add current binding to `vs` and recurse using `lcomp`
      */
    def lcomp(bs: List[Bind], body: Expr)(vs: List[String]): TExpr = bs match {
      case Nil => tcomp(body)(vs)
      case Bind(s, rhs) :: t => TLet(tcomp(rhs)(vs), lcomp(t, body)(s :: vs))
    }

    e match {
      case CstI(v) => TCstI(v)
      case Var(s) => TVar(vs.indexOf(s))
      case Let(bs, body) => lcomp(bs, body)(vs)
      case Prim(op, l, r) => TPrim(op, tcomp(l)(vs), tcomp(r)(vs))
    }
  }

  def eval(e: Expr) = teval(tcomp(e)(Nil))(Nil)

  val e1 = Let(List(Bind("x", CstI(1)), Bind("y", CstI(2))), Prim("+", Var("x"), Var("y")))
  assert(eval(e1) == 3, s"(teval . tcomp)($e1) != 3")
}
