package ideas4adoption

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen
import ideas4adoption.recursion.complication._

object simple_complication_test extends Properties("R/I 2") {
  val facrotialR = new FactorialDecompostion[Int] with Recursion
  val factorialI = new FactorialDecompostion[Int] with Iterative

  property("Factorial does not care") = forAll(Gen.posNum[Int]) { (a: Int) =>
    facrotialR.solve(a) == factorialI.solve(a)
  }

  
  val euclidR = new EuclidGCDDecompostion[Int] with Recursion
  val euclidI = new EuclidGCDDecompostion[Int] with Iterative

  property("Euclid does not care") = forAll { (a: Int, b: Int) =>
    euclidR.solve((a, b)) == euclidI.solve((a, b))
  }

  
  val euclidXR = new EuclidGCDStepsDecompostion with Recursion
  val euclidXI = new EuclidGCDStepsDecompostion with Iterative

  property("Euclid (extended) does not care") = forAll { (a: Int, b: Int) =>
    euclidXR.solve((a, b)) == euclidXI.solve((a, b))
  }
}