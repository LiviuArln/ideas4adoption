package ideas4adoption

import serious_recursion._
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen

object serious_recursion_test extends Properties("R/I 3") {
  val fibonaciR = new FibonacciDecompostion with Recursion
  val fibonaciI = new FibonacciDecompostion with Iterative

  property("Fibonaci does not care") = forAll(Gen.posNum[Int] suchThat (_ < 30)) { (a: Int) =>
    fibonaciR.solve(a) == fibonaciI.solve(a)
  }

  val hanoiR = new HanoiDecompostion with Recursion
  val hanoiI = new HanoiDecompostion with Iterative

  property("Hanoi does not care") = forAll(Gen.posNum[Int] suchThat (_ < 22)) { (a: Int) =>
    hanoiR.solve((a, 1, 2, 3)) == hanoiI.solve((a, 1, 2, 3))
  }
}