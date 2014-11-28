package ideas4adoption.recursion

import spire.math.Integral

object simple {
  def gcdEuclid[A](a: A, b: A)(implicit A: Integral[A]): A =
    if (A.isZero(b)) a
    else gcdEuclid(b, A.quot(a, b))

  trait Decomposition {
    type Problem
    type Result
    def end(p: Problem): Option[Result]
    def step(p: Problem): Problem
  }

  trait Recursion { self: Decomposition =>
    def solve(p: Problem): Result =
      end(p) match {
        case Some(result) => result
        case None         => solve(step(p))
      }
  }

  trait Iterative { self: Decomposition =>
    def solve(p: Problem): Result = {
      var currentProblem = p
      var result = end(currentProblem)
      while (result == None) {
        currentProblem = step(currentProblem)
        result = end(currentProblem)
      }
      result.get
    }
  }

  class EuclidGCDDecomposition[A](implicit A: Integral[A]) extends Decomposition {
    type Problem = (A, A)
    type Result = A
    def end(p: Problem) = if (A.isZero(p._2)) Some(p._1) else None
    def step(p: Problem) = (p._2, A.quot(p._1, p._2))
  }
}