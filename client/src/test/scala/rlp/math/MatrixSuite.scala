package rlp.math

import org.scalacheck.Gen
import org.scalactic.Equality
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.PropertyChecks

class MatrixSuite extends FlatSpec with PropertyChecks with Matchers {

  val matrices = for {
    rows <- Gen.choose(1, 10)
    cols <- Gen.choose(1, 10)
    data <- Gen.listOfN(rows*cols, Gen.choose(-1e50, 1e50))
  } yield new Matrix(rows, cols, data.toArray)

  implicit object MatrixEqualizer extends Equality[Matrix] {
    override def areEqual(a: Matrix, b: Any): Boolean = b match {
      case m: Matrix => {
        a.getRows() === m.getRows() &&
        a.getCols() === m.getCols() &&
        (a.getData(), m.getData()).zipped.forall { (x, y) => Math.abs(x-y)/x <= 1e-5}
      }
      case _ => false
    }
  }

  "A Matrix" must "remain the same after being transposed twice" in {
    forAll((matrices, "m")) { m =>
      m shouldEqual (m transpose() transposeSelf())
    }
  }

  it must "scale correctly" in {
    forAll((matrices, "m"), (Gen.choose(-1e50, 1e50), "sf")) { (m, sf) =>
      whenever(sf !== 0.0) {
        m shouldEqual ((m * sf) * (1.0/sf))
      }
    }
  }
  
  it must "add and subtract correctly" in {
    forAll((matrices, "m1"), (matrices, "m2"), (Gen.choose(1, 20), "c")) { (m1, m2) =>
      val temp = new Matrix(m1)
      
      for (_ <- 0 until c) temp += m2
      temp shouldEqual (m1 + m2*c)
      
      for (_ <- 0 until c) temp -= m2
      temp shouldEqual m1
      
      for (_ <- 0 until c) temp -= m2
      temp shouldEqual (m1 - m2*c)
    }
  }
  
  it must "compute the corrct element-wise product" {
    forAll((matrices, "m"), (Gen.choose(-1e50, 1e50), "sf")) { (m, sf) =>
      whenever(sf !== 0) {
        val s1 = new Matrix(m) fill sf
        val s2 = new Matrix(m) fill (1.0/sf)
        val zeros = new Matrix(m) fill 0
        val ones = new Matrix(m) fill 1
        
        m shouldEqual (m elemProduct s1 elemProduct s2)
        zeros shouldEqual (zeros elemProduct m)
      }
    }
  }
  
  it must "commute with all the operators required" {
    forAll((matrices, "m1"), (matrices, "m2")) { (m1, m2) =>
      (m1 + m2) shouldEqual (m2 + m1)
      (m1 - m2) shouldEqual (m2 - m1)
      (m1 * m2) shouldEqual (m2 * m1)
      (m1 elemProduct m2) shouldEqual (m2 elemProduct m1)
      
    }
  }
  
  it must "do matrix multiplication correctly" {
    // TODO: Write another test here
  }
}
