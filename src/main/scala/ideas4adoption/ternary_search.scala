package ideas4adoption

object ternary_search extends App {
  class TS(s: List[Int]) {
    val bs = s.sorted

    def search(i: Int) = search_aux((0, bs.size - 1), (0, bs.size - 1), i, 1)

    def search_aux(segmentX: (Int, Int), segmentY: (Int, Int), i: Int, depth: Int): (List[(Int, Int)],Int) = {
      //println(List.fill(depth)("  ").mkString + segmentX + " " + segmentY)
      if (segmentX._1 == segmentX._2 && segmentY._1 == segmentY._2)
        if (segmentX._1 != segmentY._1 && i == bs(segmentX._1) + bs(segmentY._1)) {
          //println(List.fill(depth)("  ").mkString + "solution")
          (List((segmentX._1, segmentY._1)),1)
        } else (Nil,1)
      else {
        val tooSmall = i < (bs(segmentX._1) + bs(segmentY._1))
        val tooLarge = i > (bs(segmentX._2) + bs(segmentY._2))
        if (tooSmall || tooLarge) (Nil,1)
        else {
          val hXl = (segmentX._1 + segmentX._2) / 2
          val hXh = if (hXl == segmentX._2) segmentX._2 else hXl + 1
          val hYl = (segmentY._1 + segmentY._2) / 2
          val hYh = if (hYl == segmentY._2) segmentY._2 else hYl + 1

          val diagonalSplit = Set(((segmentX._1, hXl), (segmentY._1, hYl)), ((hXh, segmentX._2), (hYh, segmentY._2)), ((hXh, segmentX._2), (segmentY._1, hYl)))

          val wholeSplit = diagonalSplit ++ (if (segmentX._1 == segmentY._1 && segmentX._2 == segmentY._2) Set()
          else Set(((segmentX._1, hXl), (hYh, segmentY._2))))

          val r = wholeSplit.toList.map(s => search_aux(s._1, s._2, i, depth + 1)).foldLeft((List[(Int,Int)](), 1)) {
            case ((sol1, step1),(sol2, step2)) => (sol1 ::: sol2, step1 + step2)
          }
          r
        }
      }
    }
  }

  val s = List(0, -1, 0)

  println(new TS(s).search(0))
}