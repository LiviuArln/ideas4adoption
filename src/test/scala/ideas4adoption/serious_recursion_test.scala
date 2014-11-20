package ideas4adoption

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen
import ideas4adoption.util._
import ideas4adoption.recursion.serious._

object serious_recursion_test extends Properties("R/I 3") {
  val fibonaciR = new FibonacciDecompostion with Recursion
  val fibonaciI = new FibonacciDecompostion with Iterative

  property("Fibonaci does not care") = forAll(Gen.posNum[Int] suchThat (_ < 30)) { (a: Int) =>
    fibonaciR.solve(a) == fibonaciI.solve(a)
  }

  val hanoiR = new HanoiDecompostion with Recursion
  val hanoiI = new HanoiDecompostion with Iterative

  property("Hanoi does not care") = forAll(Gen.choose(1, 22)) { (a: Int) =>
    hanoiR.solve((a, (1, 2, 3))) == hanoiI.solve((a, (1, 2, 3)))
  }

  property("I is better") = forAll(Gen.choose(15, 22)) { (a: Int) =>
    val (r1, t1) = time {
      hanoiI.solve((a, (1, 2, 3)))
    }
    val (r2, t2) = time {
      hanoiR.solve((a, (1, 2, 3)))
    }
    if (t1 != 0 && t2 != 0) println("// " + a + "->" + t1 + "/" + t2)
    t1 <= t2
  }
}