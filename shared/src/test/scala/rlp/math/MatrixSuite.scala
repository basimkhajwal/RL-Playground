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
    forAll((matrices, "m1"), (matrices, "m2"), (Gen.choose(1, 20), "c")) { (m1, m2, c) =>
      whenever(m1.getRows() == m2.getRows() && m1.getCols() == m2.getCols()) {
        val temp = new Matrix(m1)

        for (_ <- 0 until c) temp += m2
        temp shouldEqual (m1 + m2*c)

        for (_ <- 0 until c) temp -= m2
        temp shouldEqual m1

        for (_ <- 0 until c) temp -= m2
        temp shouldEqual (m1 - m2*c)
      }
    }
  }
  
  it must "compute the BEST correct element-wise product" in {
    forAll((matrices, "m"), (Gen.choose(-1e30, 1e30), "sf")) { (m, sf) =>
      whenever(sf !== 0.0+-0.1) {
        val s1 = new Matrix(m) fillWith sf
        val s2 = new Matrix(m) fillWith (1.0/sf)
        val zeros = new Matrix(m) fillWith 0

        m shouldEqual (m elemProduct s1 elemProduct s2)
        s1 shouldEqual (zeros elemProduct m)
      }
    }
  }
  
  it must "commute with all the operators required" in {
    forAll((matrices, "m1"), (matrices, "m2")) { (m1, m2) =>
      whenever(m1.getRows() == m2.getRows() && m1.getCols() == m2.getCols()) {
        (m1 + m2) shouldEqual (m2 + m1)
        (m1 - m2) shouldEqual (m2 - m1)
        (m1 elemProduct m2) shouldEqual (m2 elemProduct m1)
      }
    }
  }
  
  it must "do matrix multiplication correctly" in {
    val m1 = new Matrix(3, 3, Array(1, 2, 3, 4, 5, 6, 7, 8, 9))
    val m2 = new Matrix(3, 2, Array(1, 2, 3, 4, 5, 6))

    val r1 = new Matrix(3, 3, Array(30, 36, 42, 66, 81, 96, 102, 126, 150))
    val r2 = new Matrix(2, 2, Array(35, 44, 44, 56))
    val r3 = new Matrix(3, 3, Array(5, 11, 17, 11, 25, 39, 17, 39, 61))
    val r4 = new Matrix(3, 2, Array(22, 28, 49, 64, 76, 100))

    (m1 * m1) shouldEqual r1
    (m2.transpose() * m2) shouldEqual r2
    (m2 * m2.transpose()) shouldEqual r3
    (m1 * m2) shouldEqual r4
  }
}
