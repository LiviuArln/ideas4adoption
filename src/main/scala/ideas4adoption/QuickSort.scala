package ideas4adoption

import scala.collection.mutable.Stack

class QuickSortOnDemand(xs: Array[Int]) {
  val splits = Stack[Int](xs.length - 1)
  var lastSortedIndex = -1

  def find(index: Int) {
    def swap(i: Int, j: Int) {
      val t = xs(i); xs(i) = xs(j); xs(j) = t
    }

    def sort1(left: Int, right: Int) {
      val pivot = xs((left + right) / 2)
      var leftPointer = left
      var rightPointer = right
      while (leftPointer <= rightPointer) {
        while (xs(leftPointer) < pivot) leftPointer += 1
        while (xs(rightPointer) > pivot) rightPointer -= 1
        if (leftPointer <= rightPointer) {
          swap(leftPointer, rightPointer)
          leftPointer += 1
          rightPointer -= 1
        }
      }
      if (left < rightPointer) sort1(left, rightPointer)
      if (rightPointer < right) splits.push(leftPointer)
    }

    if (lastSortedIndex < index - 1) throw new IllegalArgumentException("")
    else if (lastSortedIndex > index - 1) xs(index)
    else {
      val result = sort1(lastSortedIndex + 1, splits.pop)
      lastSortedIndex = index
      result
    }
  }
  
  def sort = for {
    i <- 0 to splits.top
  } yield find(i)

}