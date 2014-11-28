package ideas4adoption

import org.scalacheck.Gen

object util {
  val smallNumbersList = Gen.containerOf[List, Int](Gen.choose(-200, 200))

  def atLeast(n: Int) = smallNumbersList suchThat (_.size >= n)

  implicit def pointsOrdering: Ordering[List[Int]] = new Ordering[List[Int]] {
    def compare(l1: List[Int], l2: List[Int]) = {
      l1.zip(l2).map {
        case (i1, i2) => i1.compareTo(i2)
      }.dropWhile(_ == 0).head
    }
  }

  def time[T](thunk: => T): (T, Long) = {
    val t1 = System.currentTimeMillis
    val x = thunk
    val t2 = System.currentTimeMillis
    (x, t2 - t1)
  }

  abstract class AlgorithmInspector[P, R] {
    def problemSampleGenerator(size: Int): Gen[P]
    def sampleSize: Int
    def problemSolver(problem: P): R

    def times[P, R](problemSet: Vector[P])(solver: P => R) =
      problemSet.map { p: P =>
        time {
          solver(p)
        }._2
      }

    def meanTime[P, R](problemSet: Vector[P])(solver: P => R) = {
      val ts = times(problemSet)(solver)
      ts.sum / ts.size
    }

    def sample(size: Int) =
      Gen.listOfN(sampleSize, problemSampleGenerator(size)).sample.get.to[Vector]

  }

  abstract class Statistics[P, R] extends AlgorithmInspector[P, R] {
    def problemSize: Int

    def statistics = {
      val problems = sample(problemSize)
      val ts = times(problems)(problemSolver).map(_.toDouble)
      val sum = ts.reduce(_ + _)
      val mean = sum / ts.size
      val devs = ts.map(score => (score - mean) * (score - mean))
      val stddev = Math.sqrt(devs.reduce(_ + _) / devs.size)
      (mean, stddev, problems.zip(ts))
    }

    def sigmas = {
      val (mean, stddev, problemTimes) = statistics
      problemTimes.map {
        case (problem, time) => (((time - mean) / stddev).toInt, (time, problem))
      }.groupBy(_._1).map {
        case (group, problems) => (group, problems.size, problems.map {
          case (_, (time, problem)) => (time, problem)
        })
      }.toList
    }
  }

  abstract class ExponentialSizeGrowth[P, R] extends AlgorithmInspector[P, R] {
    def minimumSize: Int
    def growthSpurts: Int

    def problemGrower = {
      def growthSpurtsStream(spurts: Int, size: Int): Stream[Vector[P]] = {
        if (spurts < 0) Stream.Empty
        else
          sample(size) #:: growthSpurtsStream(spurts - 1, size * 2)
      }
      growthSpurtsStream(growthSpurts, minimumSize)
    }

    def timeGrowtsRates[R]: Stream[Double] = {
      val pg: Stream[Double] = problemGrower.map(meanTime(_)(problemSolver).toDouble)
      pg.zip(pg.tail).map {
        case (t1, t2) =>
          println(t2)
          t2 / t1
      }
    }
  }

  def randomList(span: Int)(size: Int) = for {
    xs <- Gen.listOfN(size, Gen.choose(-span, span))
  } yield xs.to[Vector]
}