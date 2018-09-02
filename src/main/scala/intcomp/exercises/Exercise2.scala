package intcomp.exercises

import intcomp.Intcomp1.{union, minus}
import intcomp.exercises.Exercise1._

object Exercise2 {

  /**
    * list free variables `Exercise1.Expr`
    */
  def freevars(e: Expr): List[String] = {

    /**
      * `_fb` free variables in let-bindings
      *
      * 1) reverse the list and pass to sub-function
      * 2) replace Nil with Nil
      * 3) last let-bind (first after doing reverse)
      *    find free variables of expression `rhs` and remove all variable names of binding for rest of list
      *    do same for rest of the list while unionising both result
      */
    def _fb(bs: List[Bind]): List[String] = {
      def go(bs: List[Bind]): List[String] = bs match {
        case Nil => Nil
        case Bind(s, rhs) :: t => union(minus(freevars(rhs), t.map(_.s)), _fb(t))
      }
      go(bs.reverse)
    }

    e match {
      case CstI(v) => Nil
      case Var(s) => List(s)
      // free variables in let-bindings and body are total free variables in let expression
      case Let(bs, body) => union(_fb(bs), minus(freevars(body), bs.map(_.s)))
      case Prim(_, l, r) => union(freevars(l), freevars(r))
    }
  }

  val e1 = Let(List(Bind("x", Var("y")), Bind("y", Var("x"))), Prim("+", Var("x"), Var("y")))
  assert(freevars(e1) == List("y"), s"freevars ${e1} != ${List("y")}")
  val e2 = Let(List(Bind("x1", Prim("+", Var("x1"), CstI(7))), Bind("x2", Prim("+", Var("x1"), Var("x1")))), Prim("+", Var("x1"), Var("x2")))
  assert(freevars(e2) == List("x1"), s"freevars ${e2} != ${List("x1")}")
}
