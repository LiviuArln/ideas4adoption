package ideas4adoption

import ternary_search._
import org.scalacheck.Properties
import org.scalacheck.Prop._
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Test.Parameters
import org.scalacheck.Test.Parameters.Default

object ternary_search_test extends Properties("R/I 3") {
  
  override def check: Unit = check(new Default {
    override val minSuccessfulTests: Int = 1000
  })

  property("Do we have a match?") = forAll(Arbitrary.arbitrary[List[Int]] suchThat (_.size > 1) suchThat (new TS(_).search(0).size > 0)) { (s: List[Int]) =>
    val res = new TS(s).search(0)
    println("___________________")
    println(s.sorted)
    println(res)
    true
  }
}