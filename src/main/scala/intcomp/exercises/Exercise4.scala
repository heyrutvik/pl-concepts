package intcomp.exercises

import intcomp.Intcomp1._
import intcomp.Machine

object Exercise4 {

  /**
    * assemble: to translate "unified stack machine" instruction list to bytecode
    *
    * Expr => [[COMPILER]] => List[SInstr] => [[ASSEMBLER]] => List[Int] => [[VM]] => Int
    */
  def assemble(sinstrs: List[SInstr]): List[Int] = sinstrs match {
    case Nil => Nil
    case SCstlI(i) :: t => List(0, i) ::: assemble(t)
    case SVar(x) :: t => List(1, x) ::: assemble(t)                    // x is variable index in stack
    case SAdd :: t => List(2) ::: assemble(t)
    case SMul :: t => List(3) ::: assemble(t)
    case SSub :: t => List(4) ::: assemble(t)
    case SPop :: t => List(5) ::: assemble(t)
    case SSwap :: t => List(6) ::: assemble(t)
  }

  val e1 = Let("x", CstI(1), Prim("+", Var("x"), Var("x")))
  assert(
    Machine.seval(assemble(scomp(e1)(Nil)).toArray) == 2,
    s"seval using bytecode: Machine.seval $e1 != 2"
  )
}
