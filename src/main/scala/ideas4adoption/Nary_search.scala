package ideas4adoption

import ideas4adoption.ternary_search.TS

object Nary_search extends App {
  class NS(s: List[Int], dimenstions: Int) {
    val bs = s.sorted
    println("#####################")

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
      search_aux(Sector.init(bs.size - 1, dimenstions), i, 1)
    }

    def search_aux(sector: Sector, value: Int, step: Int): (List[Point], Int) = {
      //println(List.fill(step)("  ").mkString + sector)
      if (!sector.containsValue(value)) (Nil, 1)
      else if (sector.isPoint)
        if (sector.hasExactValue(value)) {
          //println(List.fill(step)("  ").mkString + "solution")
          (List(sector.toPoint), 1)
        } else (Nil, 1)
      else {
        sector.split.toList.map(s => search_aux(s, value, step + 1)).foldLeft((List[Point](), 1)) {
          case ((sol1, step1), (sol2, step2)) => (sol1 ::: sol2, step1 + step2)
        }
      }
    }

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

  //  val s = List(0, -1, 0, 1)
  //
  //  println(new NS(s, 3).search(0))

  val ss = List(7, -1, 14, -12, -8, 7, 2, -15, 8, 8, -8, -14,
    -4, -5, 7, 9, 11, -4, -15, -6, 1, -14, 4, 3, 10, -5, 2, 1,
    6, 11, 2, -2, -5, -7, -6, 2, -15, 11, -6, 8, -4, 2, 1, -1,
    4, -6, -15, 1, 5, -15, 10, 14, 9, -8, -6, 4, -6, 11, 12, -15,
    7, -1, -9, 9, -1, 0, -4, -1, -12, -2, 14, -9, 7, 0, -3, -4,
    1, -2, 12, 14, -10, 0, 5, 14, -1, 14, 3, 8, 10, -8, 8, -5, -2,
    6, -11, 12, 13, -7, -12, 8, 6, -13, 14, -2, -5, -11, 1, 3, -6)

  val z = List(4, -4, -6, 6)
  //println(new NS(z, 2).search(0))
  println(new NS(ss, 5).search(0)._2)
}