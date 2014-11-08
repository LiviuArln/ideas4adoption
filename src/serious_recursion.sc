import scala.collection.mutable.Stack
import scala.collection.mutable.HashMap

object serious_recursion {
  def fibonacci(n: Int): Int = n match {
    case 1 => 1
    case 2 => 1
    case _ => fibonacci(n - 1) + fibonacci(n - 2)
  }                                               //> fibonacci: (n: Int)Int

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

  (new FibonacciDecompostion with Recursion).solve(15)
                                                  //> res0: Int = 610
  (new FibonacciDecompostion with Iterative).solve(15)
                                                  //> res1: Int = 610

  abstract class HanoiDecompostion extends Decomposition {
    type Problem = (Int, Int, Int, Int)
    type Result = List[(Int, Int)]

    def end(p: Problem) = if (p._1 == 0) Some(Nil) else None
    def step(p: Problem) =
      List((p._1 - 1, p._4, p._3, p._2), (p._1 - 1, p._2, p._4, p._3))
    def combine(p: Problem, rs: List[Result]) =
      rs.reduceLeft((x, y) => x ::: ((p._3, p._2) :: y))
  }

  (new HanoiDecompostion with Recursion).solve((7, 1, 2, 3))
                                                  //> res2: List[(Int, Int)] = List((2,1), (2,3), (1,3), (2,1), (3,2), (3,1), (2,
                                                  //| 1), (2,3), (1,3), (1,2), (3,2), (1,3), (2,1), (2,3), (1,3), (2,1), (3,2), (
                                                  //| 3,1), (2,1), (3,2), (1,3), (1,2), (3,2), (3,1), (2,1), (2,3), (1,3), (2,1),
                                                  //|  (3,2), (3,1), (2,1), (2,3), (1,3), (1,2), (3,2), (1,3), (2,1), (2,3), (1,3
                                                  //| ), (1,2), (3,2), (3,1), (2,1), (3,2), (1,3), (1,2), (3,2), (1,3), (2,1), (2
                                                  //| ,3), (1,3), (2,1), (3,2), (3,1), (2,1), (2,3), (1,3), (1,2), (3,2), (1,3), 
                                                  //| (2,1), (2,3), (1,3), (2,1), (3,2), (3,1), (2,1), (3,2), (1,3), (1,2), (3,2)
                                                  //| , (3,1), (2,1), (2,3), (1,3), (2,1), (3,2), (3,1), (2,1), (3,2), (1,3), (1,
                                                  //| 2), (3,2), (1,3), (2,1), (2,3), (1,3), (1,2), (3,2), (3,1), (2,1), (3,2), (
                                                  //| 1,3), (1,2), (3,2), (3,1), (2,1), (2,3), (1,3), (2,1), (3,2), (3,1), (2,1),
                                                  //|  (2,3), (1,3), (1,2), (3,2), (1,3), (2,1), (2,3), (1,3), (2,1), (3,2), (3,1
                                                  //| ), (2,1), (3,2), (1,3), (1,2), (3,2), (3,1), (2,1), (2,3), (1,3), (2,1), (3
                                                  //| ,2), (3,1), (2,1))
  (new HanoiDecompostion with Iterative).solve((7, 1, 2, 3))
                                                  //> res3: List[(Int, Int)] = List((2,1), (2,3), (1,3), (2,1), (3,2), (3,1), (2,
                                                  //| 1), (2,3), (1,3), (1,2), (3,2), (1,3), (2,1), (2,3), (1,3), (2,1), (3,2), (
                                                  //| 3,1), (2,1), (3,2), (1,3), (1,2), (3,2), (3,1), (2,1), (2,3), (1,3), (2,1),
                                                  //|  (3,2), (3,1), (2,1), (2,3), (1,3), (1,2), (3,2), (1,3), (2,1), (2,3), (1,3
                                                  //| ), (1,2), (3,2), (3,1), (2,1), (3,2), (1,3), (1,2), (3,2), (1,3), (2,1), (2
                                                  //| ,3), (1,3), (2,1), (3,2), (3,1), (2,1), (2,3), (1,3), (1,2), (3,2), (1,3), 
                                                  //| (2,1), (2,3), (1,3), (2,1), (3,2), (3,1), (2,1), (3,2), (1,3), (1,2), (3,2)
                                                  //| , (3,1), (2,1), (2,3), (1,3), (2,1), (3,2), (3,1), (2,1), (3,2), (1,3), (1,
                                                  //| 2), (3,2), (1,3), (2,1), (2,3), (1,3), (1,2), (3,2), (3,1), (2,1), (3,2), (
                                                  //| 1,3), (1,2), (3,2), (3,1), (2,1), (2,3), (1,3), (2,1), (3,2), (3,1), (2,1),
                                                  //|  (2,3), (1,3), (1,2), (3,2), (1,3), (2,1), (2,3), (1,3), (2,1), (3,2), (3,1
                                                  //| ), (2,1), (3,2), (1,3), (1,2), (3,2), (3,1), (2,1), (2,3), (1,3), (2,1), (3
                                                  //| ,2), (3,1), (2,1))

  class BinarySearchDecompostion {
    type Problem = (List[Int], Int)
    type Result = Boolean

    def end(p: Problem) = if (p._1.size == 1) Some(p._1(0) == p._2) else None
    def step(p: Problem) = {
      val pivot = p._1.size / 2
      p._1(pivot) compare p._2 match {
        case 0 => List(List(p._1(pivot)),p._2)
        case 1 => List(p._1.take(pivot-1),p._2)
        case -1 => List(p._1.takeRight(p._1.size - pivot),p._2)
      }
    }
    def combine(p: Problem, rs: List[Result]) = rs
  }

}