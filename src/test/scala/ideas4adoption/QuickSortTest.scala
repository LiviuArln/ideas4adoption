package ideas4adoption

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen
import ideas4adoption.util._
import ideas4adoption.sort.QuickSortOnDemand

object QuickSortTest extends Properties("QuickSort") {
  println(new QuickSortOnDemand(List(2, 0, 1).toArray).sort.toList)
  
  property("Does not miss a thing") = forAll { (l: List[Int]) =>
    val q = new QuickSortOnDemand(l.toArray).sort
    q.size == l.size
  }

  property("Sorts correctly") = forAll { (l: List[Int]) =>
    val q = new QuickSortOnDemand(l.toArray).sort
    q.toList.equals(l.sorted)
  }

  

}