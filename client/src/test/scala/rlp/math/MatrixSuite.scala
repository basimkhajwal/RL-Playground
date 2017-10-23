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

  it must "scale properly" in {
    forAll((matrices, "m"), (Gen.choose(-1e50, 1e50), "sf")) { (m, sf) =>
      whenever(sf !== 0.0) {
        m shouldEqual ((m * sf) * (1.0/sf))
      }
    }
  }
}
