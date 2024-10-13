package example

import org.atnos.eff.Eff
import org.atnos.eff.Fx
import org.atnos.eff.EvalEffect
import org.atnos.eff.syntax.eval._
import org.atnos.eff.syntax.eff._
import cats.syntax.all._
import cats.Eval

object Main {

  type E = Fx.fx1[Eval]

  val list = (1 to 5000).toList

  def stacksafeRun(): Unit = {
    val action = list.traverse(i => EvalEffect.delay(i))
    assert(action.runEval.run == list)
  }

  def stacksafeAttempt(): Unit = {
    val action = list.traverse(i => EvalEffect.delay(i))
    assert(action.attemptEval.run == Right(list))
  }

  def stacksafeRecursion(): Unit = {
    def loop(i: Int): Eval[Eff[Fx.fx1[Eval], Int]] =
      if (i == 0) {
        Eval.now(Eff.pure(1))
      } else {
        Eval.now(org.atnos.eff.eval.defer(loop(i - 1)).map(_ + 1))
      }

    assert(loop(100000).value.runEval.run == 100001)
  }

  def main(args: Array[String]) : Unit = {
    stacksafeRecursion()
    stacksafeRun()
    stacksafeAttempt()
  }
}
