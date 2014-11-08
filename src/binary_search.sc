object binary_search {
  val s = List(1, 3, 545, 32, 4, 776, 23, 12, 76)

  class BS(s: List[Int]) {
    val bs = s.sorted
    def search(i: Int) = search_aux(0, bs.size - 1, i)

    def search_aux(idx1: Int, idx2: Int, i: Int): Boolean = {
      println(idx1, idx2)
      if (idx1 == idx2) i == bs(idx1)
      else if (i < bs(idx1) || i > bs(idx2)) false
      else {
        val h = (idx2 - idx1) / 2
        search_aux(idx1, h, i) || search_aux(h + 1, idx2, i)
      }
    }
  }

  new BS(s).search(776)
  

}