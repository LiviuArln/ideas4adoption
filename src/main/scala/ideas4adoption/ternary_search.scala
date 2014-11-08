package ideas4adoption

object ternary_search extends App {
  class TS(s: List[Int]) {
    val bs = s.sorted

    def search(i: Int) = search_aux((0, bs.size - 1), (0, bs.size - 1), i)

    def search_aux(segmentX: (Int, Int), segmentY: (Int, Int), i: Int): List[(Int, Int)] = {
      if (segmentX._1 == segmentX._2 && segmentY._1 == segmentY._2)
        if (segmentX._1 != segmentY._1 && i == bs(segmentX._1) + bs(segmentY._1))
          List((segmentX._1, segmentY._1)) else Nil
      else {
        val tooSmall = i < (bs(segmentX._1) + bs(segmentY._1))
        val tooLarge = i > (bs(segmentX._2) + bs(segmentY._2))
        if (tooSmall || tooLarge) Nil
        else {
          val hXl = (segmentX._2 + segmentX._1) / 2
          val hXh = if (hXl == segmentX._2) segmentX._2 else hXl + 1
          val hYl = (segmentY._2 + segmentY._1) / 2
          val hYh = if (hYl == segmentY._2) segmentY._2 else hYl + 1
          search_aux((segmentX._1, hXl), (segmentY._1, hYl), i) :::
            search_aux((hXh, segmentX._2), (segmentY._1, hYl), i) :::
            search_aux((hXh, segmentX._2), (hYh, segmentY._2), i)
        }
      }
    }
  }

  val s = List(0, -1, 0)

  new TS(s).search(0)
}