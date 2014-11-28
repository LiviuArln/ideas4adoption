package ideas4adoption.sum

object ternary_search_nice extends App {
  class TSN(s: List[Int]) {
    val bs = s.sorted

    case class Segment(min: Int, max: Int) {
      def split = {
        val halfLow = (min + max) / 2
        val halfHigh = if (halfLow == max) max else halfLow + 1
        (Segment(min, halfLow), Segment(halfHigh, max))
      }

      def isPoint = min == max

      def sameOrigin(s: Segment) = min == s.min
      def isDiagonal(s: Segment) = sameOrigin(s) && max == s.max
    }

    def search(i: Int) = {
      val x = Segment(0, bs.size - 1)
      val y = Segment(0, bs.size - 1)
      search_aux(x, y, i, 1)
    }

    def search_aux(segmentX: Segment, segmentY: Segment, i: Int, depth: Int): (List[(Int, Int)], Int) = {
      //println(List.fill(depth)("  ").mkString + segmentX + " " + segmentY)
      if (segmentX.isPoint && segmentY.isPoint)
        if (!segmentX.sameOrigin(segmentY) && i == bs(segmentX.min) + bs(segmentY.min)) {
          //println(List.fill(depth)("  ").mkString + "solution")
          (List((segmentX.min, segmentY.min)), 1)
        } else (Nil, 1)
      else {
        val tooSmall = i < (bs(segmentX.min) + bs(segmentY.min))
        val tooLarge = i > (bs(segmentX.max) + bs(segmentY.max))
        if (tooSmall || tooLarge) (Nil, 1)
        else {
          val splitX = segmentX.split
          val splitY = segmentY.split

          val diagonalSplit = Set((splitX._1, splitY._1), (splitX._2, splitY._2), (splitX._2, splitY._1))

          val wholeSplit = diagonalSplit ++ (if (segmentX.isDiagonal(segmentY)) Set()
          else Set((splitX._1, splitY._2)))

          val r = wholeSplit.toList.map(s => search_aux(s._1, s._2, i, depth + 1)).foldLeft((List[(Int, Int)](), 1)) {
            case ((sol1, step1), (sol2, step2)) => (sol1 ::: sol2, step1 + step2)
          }
          r
        }
      }
    }
  }

  val s = List(0, 0)

  new TSN(s).search(0)
}