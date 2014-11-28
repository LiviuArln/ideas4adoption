package ideas4adoption

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import ideas4adoption.recursion.simple._

object simple_recursion_test extends Properties("R/I 1") {
  
  val r = (new EuclidGCDDecomposition[Int] with Recursion)
  val i = (new EuclidGCDDecomposition[Int] with Iterative)

  property("Euclid does not care") = forAll { p: r.Problem =>
    r.solve(p) == i.solve(p)
  }
}
