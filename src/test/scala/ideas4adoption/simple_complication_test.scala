package ideas4adoption

import simple_complication._
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen

object simple_complication_test extends Properties("R/I 2") {
  val facrotialR = new FactorialDecompostion with Recursion
  val factorialI = new FactorialDecompostion with Iterative

  property("Factorial does not care") = forAll(Gen.posNum[Int]) { (a: Int) =>
    facrotialR.solve(a) == factorialI.solve(a)
  }

  
  val euclidR = new EuclidGCDDecompostion with Recursion
  val euclidI = new EuclidGCDDecompostion with Iterative

  property("Euclid does not care") = forAll { (a: Int, b: Int) =>
    euclidR.solve((a, b)) == euclidI.solve((a, b))
  }

  
  val euclidXR = new EuclidGCDStepsDecompostion with Recursion
  val euclidXI = new EuclidGCDStepsDecompostion with Iterative

  property("Euclid (extended) does not care") = forAll { (a: Int, b: Int) =>
    euclidXR.solve((a, b)) == euclidXI.solve((a, b))
  }
}