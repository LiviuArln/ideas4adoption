package ideas4adoption.recursion

import spire.math.Integral

object complication {
  def factorial[A](n: A)(implicit A: Integral[A]): A = {
    if (n.equals(A.one)) A.one
    else A.times(n, factorial(A.minus(n, A.one)))
  }

  trait Decomposition {
    type Problem
    type Result

    def end(p: Problem): Option[Result]
    def step(p: Problem): Problem
    def combine(p: Problem, r: Result): Result
  }

  trait Recursion { self: Decomposition =>
    def solve(p: Problem): Result =
      end(p) match {
        case Some(result) => result
        case None         => combine(p, solve(step(p)))
      }

  }

  trait Iterative { self: Decomposition =>
    def solve(p: Problem): Result = {
      var currentProblem = p
      var lastResult = end(currentProblem)
      var oldProblems: List[Problem] = Nil
      while (lastResult == None) {
        oldProblems = currentProblem :: oldProblems
        currentProblem = step(currentProblem)
        lastResult = end(currentProblem)
      }
      oldProblems.reverse.foldRight(lastResult.get)(combine(_, _))
    }
  }

  class FactorialDecompostion[A](implicit A: Integral[A]) extends Decomposition {
    type Problem = A
    type Result = A

    def end(p: Problem) = if (p.equals(A.one)) Some(A.one) else None
    def step(p: Problem) = A.minus(p, A.one)
    def combine(p: Problem, r: Result) = A.times(p, r)
  }

  class EuclidGCDStepsDecompostion extends Decomposition {
    type Problem = (Int, Int)
    type Result = (Int, Int)

    def end(p: Problem) = if (p._2 == 0) Some((p._1, 0)) else None
    def step(p: Problem) = (p._2, p._1 % p._2)
    def combine(p: Problem, r: Result) = (r._1, r._2 + 1)
  }

  class EuclidGCDDecompostion[A](implicit A: Integral[A]) extends Decomposition {
    type Problem = (A, A)
    type Result = A

    def end(p: Problem) = if (A.isZero(p._2)) Some(p._1) else None
    def step(p: Problem) = (p._2, A.quot(p._1, p._2))
    def combine(p: Problem, r: Result) = r
  }
}