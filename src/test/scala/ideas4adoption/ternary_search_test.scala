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

object ternary_search_test extends Properties("AA 1") {

  //  override def mainRunner(args: Array[String]): Int = {
  //    val res = Test.checkProperties(Parameters.default.withMinSuccessfulTests(100) , this)
  //    val failed = res.filter(!_._2.passed).size
  //    failed
  //  }

  val smallNumbersList = Gen.containerOf[List, Int](Gen.choose(-200, 200)) suchThat (_.size > 1)

  property("Do we have a match?") = forAll(smallNumbersList) { (s: List[Int]) =>
    val res = new TS(s).search(0)
    println("___________________")
    collect(s.size * 10 / res._2) {
      println(s.sorted)
      println(res._1)
      true
    }
  }

  property("Same thing") = forAll(smallNumbersList) { (s: List[Int]) =>
    new TS(s).search(0) == new TSN(s).search(0)
  }
  
  property("Same but ... more abstract") = forAll(smallNumbersList) { (s: List[Int]) =>
    new TS(s).search(0)._1.eq(new NS(s,2).search(0))
  }
}