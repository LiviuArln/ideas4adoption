package ideas4adoption

import simple_recursion._
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

object simple_recursion_test extends Properties("R/I 1") {
  val r = (new EuclidGCDDecomposition with Recursion)
  val i = (new EuclidGCDDecomposition with Iterative)

  property("Euclid does not care") = forAll { (a: Int, b: Int) =>
    r.solve((a, b)) == i.solve((a, b))
  }

}