package ideas4adoption.sum

import ideas4adoption.recursion.serious._

object NSum {
  abstract class WithDecomposition(s: List[Int], dimenstions: Int) {
    val bs = s.sorted

    object Sector {
      def init(zeroTo: Int, dimensions: Int) = Sector(List.fill(dimensions)(Segment(0, zeroTo)), fullSplit(dimensions).toSet, diagonalSplit(dimensions).toSet)

      def fullSplit(dimensions: Int) = (0 to Math.pow(2, dimensions).toInt - 1).toList.map {
        _.toBinaryString.toList.map(_.toString.toInt)
      }.map { b =>
        List.fill(dimensions - b.size)(0) ::: b
      }

      def diagonalSplit(dimensions: Int) = (0 to dimensions).toList.map { i =>
        List.fill(i)(1) ::: List.fill(dimensions - i)(0)
      }
    }

    type Point = List[Int]

    def search(i: Int) = {
      solve(Sector.init(bs.size - 1, dimenstions), i, 1)
    }

    type Problem = (Sector, Int, Int)
    type Result = (List[Point], Int)

    def end(p: Problem): Option[Result] =
      if (!p._1.containsValue(p._2)) Some((Nil, 1))
      else if (p._1.isPoint)
        if (p._1.hasExactValue(p._2)) {
          Some((List(p._1.toPoint), 1))
        } else Some((Nil, 1))
      else None

    def step(p: Problem): List[Problem] =
      p._1.split.toList.map((_, p._2, p._3 + 1))

    def combine(p: Problem, rs: List[Result]): Result =
      rs.foldLeft((List[Point](), 1)) {
        case ((sol1, step1), (sol2, step2)) => (sol1 ::: sol2, step1 + step2)
      }

    def solve(p: Problem): Result

    case class Sector(segments: List[Segment], fullSplit: Set[List[Int]], diagonalSplit: Set[List[Int]]) {

      //println(segments)
      def isPoint = segments.forall(_.isPoint)
      def min = segments.map(s => bs(s.min)).sum
      def max = segments.map(s => bs(s.max)).sum
      def hasExactValue(value: Int) = segments.size == segments.distinct.size && min == value
      def containsValue(value: Int) = min <= value && value <= max
      def toPoint = segments.map(_.min)
      def isDiagonal = segments.forall(_.min == segments(0).min) && segments.forall(_.max == segments(0).max)
      def split = {
        val segmentSplits = segments.map(_.split)
        val splitSchema = if (isDiagonal) diagonalSplit else fullSplit
        val newSectors = splitSchema.map { s =>
          segmentSplits.zip(s).map {
            case (segmentSplit, digit) => segmentSplit(digit)
          }
        }
        newSectors.map(Sector(_, fullSplit, diagonalSplit))
      }
    }

    case class Segment(min: Int, max: Int) {
      def split = {
        val halfLow = (min + max) / 2
        val halfHigh = if (halfLow == max) max else halfLow + 1
        List(Segment(min, halfLow), Segment(halfHigh, max))
      }

      def isPoint = min == max
    }
  }

  type NSumDecR = WithDecomposition with Recursion
  type NSumDecI = WithDecomposition with Iterative
}