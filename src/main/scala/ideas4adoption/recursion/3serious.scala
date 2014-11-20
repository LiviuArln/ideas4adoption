package ideas4adoption.recursion

import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack

object serious {
  def fibonacci(n: Int): Int = n match {
    case 1 => 1
    case 2 => 1
    case _ => fibonacci(n - 1) + fibonacci(n - 2)
  }

  trait Decomposition {
    type Problem
    type Result

    def end(p: Problem): Option[Result]
    def step(p: Problem): List[Problem]
    def combine(p: Problem, rs: List[Result]): Result
    def solve(p: Problem): Result
  }

  trait Recursion extends Decomposition {
    def solve(p: Problem): Result =
      end(p) match {
        case Some(result) => result
        case None => combine(p, step(p).map(solve))
      }
  }

  trait Iterative extends Decomposition {
    def solve(p: Problem): Result = {
      val problemStack = new Stack[Problem]
      problemStack push p
      val stepMap: HashMap[Problem, List[Problem]] = new HashMap
      val resultsMap: HashMap[Problem, Result] = new HashMap
      while (problemStack.nonEmpty) {
        val currentProblem = problemStack.pop
        stepMap.get(currentProblem) match {
          case None =>
            end(currentProblem) match {
              case None => {
                val subProblems = step(currentProblem)
                problemStack push currentProblem
                problemStack pushAll subProblems
                stepMap.put(currentProblem, subProblems)
              }
              case Some(result) => resultsMap.put(currentProblem, result)
            }
          case Some(subProblems) => {
            resultsMap.put(currentProblem, combine(currentProblem, subProblems.map(resultsMap.get(_).get)))
          }
        }
      }
      resultsMap.get(p).get
    }
  }

  class FibonacciDecompostion {
    type Problem = Int
    type Result = Int

    def end(p: Problem) = if (p == 1 || p == 2) Some(1) else None
    def step(p: Problem) = List(p - 1, p - 2)
    def combine(p: Problem, rs: List[Result]) = rs.reduceLeft(_ + _)
  }

  abstract class HanoiDecompostion extends Decomposition {
    type Problem = (Int, (Int, Int, Int))
    type Result = List[(Int, Int)]

    def end(p: Problem) = if (p._1 == 0) Some(Nil) else None
    def step(p: Problem) =
      List((p._1 - 1, (p._2._3, p._2._2, p._2._1)), (p._1 - 1, (p._2._1, p._2._3, p._2._2)))
    def combine(p: Problem, rs: List[Result]) =
      rs.reduceLeft((x, y) => x ::: ((p._2._2, p._2._1) :: y))
  }

}