package ideas4adoption.sum

import ideas4adoption.util._

object classic3sum extends App {
  def _3sum(s: Vector[Int]) = {
    val ss = s.sorted
    val n = ss.length
    var result = Vector[(Int, Int, Int)]()
    (0 to n - 3).foreach { i =>
      val a = ss(i)
      var start = i + 1
      var end = n - 1
      while (start < end) {
        val b = ss(start)
        val c = ss(end)
        if (a + b + c == 0) {
          result +:= (a, b, c)
          start += 1
          end -= 1
        } else if (a + b + c > 0) {
          end -= 1
        } else {
          start += 1
        }
      }
    }
    result
  }

  type P = Vector[Int]
  type R = Vector[(Int, Int, Int)]

  trait _3SumInspector {
    val minimumSize = 100
    val growthSpurts = 5
    val sampleSize = 100

    def problemSampleGenerator(size: Int) = randomList(1000)(size)

    def problemSolver(problem: P) = _3sum(problem)
  }

  val growth = new ExponentialSizeGrowth[P, R] with _3SumInspector

  //growth.timeGrowtsRates.foreach(r => println("r " + r))

  val stats = new Statistics[P, R] with _3SumInspector {
    val problemSize = 1000
  }
  
  stats.sigmas.sortBy(_._1).foreach(println)
}