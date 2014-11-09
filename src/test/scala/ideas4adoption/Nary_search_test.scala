package ideas4adoption

import ternary_search._
import ternary_search_nice._
import org.scalacheck.Properties
import org.scalacheck.Prop._
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Test.Parameters
import org.scalacheck.Test.Parameters.Default
import org.scalacheck.Test
import ideas4adoption.Nary_search.NS

object Nary_search_test extends Properties("AA 2") {
  
  val smallNumbersList = Gen.containerOf[List, Int](Gen.choose(-200, 200)) 
  def atLeast(n:Int) = smallNumbersList suchThat (_.size >= n)

  implicit def pointsOrdering: Ordering[List[Int]] = new Ordering[List[Int]] {
    def compare(l1: List[Int], l2: List[Int]) = {
      l1.zip(l2).map {
        case (i1, i2) => i1.compareTo(i2)
      }.dropWhile(_ == 0).head
    }
  }

  property("Same but ... more abstract") = forAll(atLeast(2)) { (s: List[Int]) =>
    val r1 = new TS(s).search(0)
    val r2 = new NS(s, 2).search(0)
    r1._1.sorted.equals(r2._1.sorted) && r1._2 == r2._2
  }

  property("Do we have a match?") = forAll(atLeast(3)) { (s: List[Int]) =>
    val res = new NS(s, 3).search(0)
    println("___________________")
    collect(s.size * 10 / res._2) {
      println(s.sorted)
      println(res._1)
      true
    }
  }
}