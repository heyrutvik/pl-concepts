package intcomp.exercises

import intcomp.exercises.Exercise1._
import intro.Intro2.{Env, lookup}

object Exercise6 {

  /**
    * interpret let-bindings "simultaneous" rather than "sequential"
    *
    * The idea is that all the right-hand side expressions should be evaluated,
    * after which all the variables are bound to those values simultaneously.
    * Hence
    * let x = 11 in let x = 22 y = x+1 in x+y end end
    * should compute 12 + 22 because x in x+1 is the outer x (and hence is 11),
    * and x in x+y is the inner x (and hence is 22).
    */
  def eval(e: Expr)(env: Env): Int = e match {
    case CstI(v) => v
    case Var(s) => lookup(env)(s)
    case Let(bs, body) => eval(body) { bs.map {                        // execute all `rhs` using current `env` and
        case Bind(b, rhs) => (b, eval(rhs)(env))                       // add name-value binding to `env`
      } ::: env
    }
    case Prim("+", l, r) => eval(l)(env) + eval(r)(env)
    case Prim("*", l, r) => eval(l)(env) * eval(r)(env)
    case Prim("-", l, r) => eval(l)(env) - eval(r)(env)
    case _ => sys.error("unknown expression")
  }

  val e1 = Let(List(Bind("x", CstI(11))),
    Let(List(Bind("x", CstI(22)), Bind("y", Prim("+", Var("x"), CstI(1)))),
      Prim("+", Var("x"), Var("y"))
    )
  )
  assert(eval(e1)(Nil) == 34, s"simultaneous let-bindings eval $e1 != 32")
}
