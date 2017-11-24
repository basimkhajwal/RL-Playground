package rlp.math

import org.scalacheck.Gen
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.PropertyChecks

class MatrixSuite extends FlatSpec with PropertyChecks with Matchers {

  import rlp._

  val matrices: Gen[Matrix] = for {
    rows <- Gen.choose(1, 10)
    cols <- Gen.choose(1, 10)
    data <- Gen.listOfN(rows*cols, Gen.choose(-1e50, 1e50))
  } yield new Matrix(rows, cols, data.toArray)

  val sameSizeMatrices: Gen[(Matrix, Matrix)] = for {
    m1 <- matrices
    d2 <- Gen.listOfN(m1.rows * m1.cols, Gen.choose(-1e50, 1e50))
  } yield (m1, new Matrix(m1.rows, m1.cols, d2.toArray))


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
    forAll((sameSizeMatrices, "m1 & m2"), (Gen.choose(1, 20), "c")) { (ms, c) =>
      val (m1, m2) = ms
      val temp = new Matrix(m1)

      for (_ <- 0 until c) temp += m2
      temp shouldEqual (m1 + m2*c)

      for (_ <- 0 until c) temp -= m2
      temp shouldEqual m1

      for (_ <- 0 until c) temp -= m2
      temp shouldEqual (m1 - m2*c)
    }
  }
  
  it must "compute the correct element-wise product" in {
    forAll((matrices, "m"), (Gen.choose(-1e50, 1e50), "sf")) { (m, sf) =>
      whenever(sf !== 0.0) {
        val s1 = new Matrix(m) fillWith sf
        val s2 = new Matrix(m) fillWith (1.0/sf)
        val zeros = new Matrix(m) fillWith 0

        m shouldEqual (m elemProduct s1 elemProduct s2)
        zeros shouldEqual (zeros elemProduct m)
      }
    }
  }
  
  it must "commute with all the operators required" in {
    forAll((sameSizeMatrices, "m1 & m2")) { ms =>
      val (m1, m2) = ms
      (m1 + m2) shouldEqual (m2 + m1)
      (m1 - m2) shouldEqual (m2 - m1).each(_ * -1)
      (m1 elemProduct m2) shouldEqual (m2 elemProduct m1)
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

