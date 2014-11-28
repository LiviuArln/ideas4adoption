package ideas4adoption

import org.scalacheck.Properties
import org.scalacheck.Prop._
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Test.Parameters
import org.scalacheck.Test.Parameters.Default
import org.scalacheck.Test
import ideas4adoption.util._
import ideas4adoption.recursion.serious._
import ideas4adoption.sum.ternary_search._
import ideas4adoption.sum.ternary_search_nice._
import ideas4adoption.sum.NSum._
import ideas4adoption.sum.Nary_search._

object NSum_test extends Properties("AA 3") {
  property("Same but diferent R2") = forAll(atLeast(2)) { (s: List[Int]) =>
    val ns = new NS(s, 2)
    val wd = new WithDecomposition(s, 2) with Recursion
    val r1 = ns.search(0)
    val r2 = wd.search(0)
    r1.sorted.equals(r2._1.sorted) && ns.Sector.sectorsGenerated == r2._2
  }

  property("Same but diferent R3") = forAll(atLeast(3)) { (s: List[Int]) =>
    val ns = new NS(s, 3)
    val r1 = ns.search(0)
    val r2 = (new WithDecomposition(s, 3) with Recursion).search(0)
    r1.sorted.equals(r2._1.sorted) && ns.Sector.sectorsGenerated == r2._2
  }

  property("Same but diferent R4") = forAll(atLeast(4)) { (s: List[Int]) =>
    val ns = new NS(s, 4)
    val r1 = ns.search(0)
    val r2 = (new WithDecomposition(s, 4) with Recursion).search(0)
    r1.sorted.equals(r2._1.sorted) && ns.Sector.sectorsGenerated == r2._2
  }

  property("Same but diferent IR2") = forAll(atLeast(2)) { (s: List[Int]) =>
    val r1 = (new WithDecomposition(s, 2) with Iterative).search(0)
    val r2 = (new WithDecomposition(s, 2) with Recursion).search(0)
    r1._1.sorted.equals(r2._1.sorted) && r1._2 == r2._2
  }

  property("Same but diferent IR3") = forAll(atLeast(3)) { (s: List[Int]) =>
    val r1 = (new WithDecomposition(s, 3) with Iterative).search(0)
    val r2 = (new WithDecomposition(s, 3) with Recursion).search(0)
    r1._1.sorted.equals(r2._1.sorted) && r1._2 == r2._2
  }

  property("Same but diferent IR4") = forAll(atLeast(4)) { (s: List[Int]) =>
    val r1 = (new WithDecomposition(s, 4) with Iterative).search(0)
    val r2 = (new WithDecomposition(s, 4) with Recursion).search(0)
    r1._1.sorted.equals(r2._1.sorted) && r1._2 == r2._2
  }

  property("R is better") = forAll(atLeast(40)) { (s: List[Int]) =>
    val (r1, t1:Long) = time {
      (new WithDecomposition(s, 4) with Iterative).search(0)
    }
    val (r2, t2:Long) = time {
      (new WithDecomposition(s, 4) with Recursion).search(0)
    }
    true
  }

}