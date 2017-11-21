import org.scalactic.Equality
import rlp.math.Matrix

package object rlp {

  implicit object MatrixEqualizer extends Equality[Matrix] {
    override def areEqual(a: Matrix, b: Any): Boolean = b match {
      case m: Matrix => {
        a.getRows() == m.getRows() &&
          a.getCols() == m.getCols() &&
          (a.getData(), m.getData()).zipped.forall { (x, y) => x == y || Math.abs(x-y) <= 1e-2 || Math.abs((x-y)/x) <= 1e-2}
      }
      case _ => false
    }
  }

}
