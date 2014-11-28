package ideas4adoption

import org.scalacheck.Properties
import org.scalacheck.Prop._
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Test.Parameters
import org.scalacheck.Test.Parameters.Default
import org.scalacheck.Test
import ideas4adoption.util._
import ideas4adoption.sum.ternary_search._
import ideas4adoption.sum.Nary_search.NS

object Nary_search_test extends Properties("AA 2") {

  property("Same but ... more abstract") = forAll(atLeast(2)) { (s: List[Int]) =>
    val r1 = new TS(s).search(0)
    val ns = new NS(s, 2)
    val r2 = ns.search(0)
    r1._1.sorted.equals(r2.sorted) && r1._2 == ns.Sector.sectorsGenerated
  }

  property("Cube") = forAll(atLeast(3)) { (s: List[Int]) =>
    val ns = new NS(s, 3)
    val res = ns.search(0)
    collect(s.size * 10 / ns.Sector.sectorsGenerated) {
      true
    }
  }

  property("Hypercube 4") = forAll(atLeast(4)) { (s: List[Int]) =>
    val ns = new NS(s, 4)
    val res = ns.search(0)
    collect(s.size * 10 / ns.Sector.sectorsGenerated) {
      true
    }
  }

//  property("Hypercube 5") = forAll(atLeast(5)) { (s: List[Int]) =>
//    val ns = new NS(s, 5)
//    val res = ns.search(0)
//    collect(ns.Sector.sectorsGenerated / Math.pow(s.size, 5) , "sectors/(size^5) ratio") {
//      true
//    }
//  }
  
  properties.foreach(println)
}