object simple_recursion {
  def gcdEuclid(a: Int, b: Int): Int = b match {
    case 0 => a
    case _ => gcdEuclid(b, a % b)
  }                                               //> gcdEuclid: (a: Int, b: Int)Int

  gcdEuclid(33, 27)                               //> res0: Int = 3

  trait Decomposition {
    type Problem
    type Result

    def end(p: Problem): Option[Result]
    def step(p: Problem): Problem
  }

  trait Recursion extends Decomposition {
    def solve(p: Problem): Result =
      end(p) match {
        case Some(result) => result
        case None => solve(step(p))
      }
  }

  trait Iterative extends Decomposition {
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

  class EuclidGCDDecomposition {
    type Problem = (Int, Int)
    type Result = Int
    def end(p: Problem) = if (p._2 == 0) Some(p._1) else None
    def step(p: Problem) = (p._2, p._1 % p._2)
  }

 

}