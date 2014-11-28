package ideas4adoption.sort

import scala.collection.mutable.Stack

class QuickSortOnDemand(xs: Array[Int]) {
  val splits = Stack[Int](xs.length - 1)
  var lastSortedIndex = -1

  def find(index: Int) = {
    xs.foreach(x=>println(">" + x))
    println(index)
    def swap(i: Int, j: Int) {
      val t = xs(i); xs(i) = xs(j); xs(j) = t
    }

    def fractionSort(left: Int, right: Int) {
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
      if (left < rightPointer) fractionSort(left, rightPointer)
      if (rightPointer < right) splits.push(leftPointer)
    }

    if (lastSortedIndex < index - 1) throw new IllegalArgumentException("")
    else if (lastSortedIndex == index - 1) {
      fractionSort(lastSortedIndex + 1, splits.pop)
      splits.foreach { x => println("/" + x) }
      lastSortedIndex = index
    }
    xs.foreach(x=>println("<" + x))
    xs(index)
  }

  def sort: Stream[Int] = {
    def sort_aux(from: Int): Stream[Int] =
      if (from == xs.size) Stream.Empty
      else find(from) #:: sort_aux(from + 1)

    sort_aux(0)
  }

}

object X extends App {
  val x = new QuickSortOnDemand(List(2, 0, 1).toArray)
  val f = x.find(0)
  println(f)
  println(x.lastSortedIndex)
  println(x.splits.foreach { x => println("/" + x) })
  println(x.find(1))
  println(x.find(2))

}