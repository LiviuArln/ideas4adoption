package ideas4adoption.recursion

object complication {
  def factorial(n: Int): Int = n match {
    case 1 => 1
    case _ => n * factorial(n - 1)
  }

  trait Decomposition {
    type Problem
    type Result

    def end(p: Problem): Option[Result]
    def step(p: Problem): Problem
    def combine(p: Problem, r: Result): Result
  }

  trait Recursion extends Decomposition {
    def solve(p: Problem): Result =
      end(p) match {
        case Some(result) => result
        case None => combine(p, solve(step(p)))
      }

  }

  trait Iterative extends Decomposition {
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

  class FactorialDecompostion {
    type Problem = Int
    type Result = Int

    def end(p: Int) = if (p == 1) Some(1) else None
    def step(p: Int) = p - 1
    def combine(p: Int, r: Int) = p * r
  }

  class EuclidGCDStepsDecompostion {
    type Problem = (Int, Int)
    type Result = (Int, Int)

    def end(p: Problem) = if (p._2 == 0) Some((p._1, 0)) else None
    def step(p: Problem) = (p._2, p._1 % p._2)
    def combine(p: Problem, r: Result) = (r._1, r._2 + 1)
  }

  class EuclidGCDDecompostion {
    type Problem = (Int, Int)
    type Result = Int

    def end(p: Problem) = if (p._2 == 0) Some(p._1) else None
    def step(p: Problem) = (p._2, p._1 % p._2)
    def combine(p: Problem, r: Result) = r
  }
}