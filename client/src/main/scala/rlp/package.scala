import com.thoughtworks.binding.Binding
import org.scalajs.dom.raw
import org.scalajs.dom.document

package object rlp {

  implicit def makeIntellijHappy[T<:raw.Node](x: scala.xml.Node): Binding[T] =
    throw new AssertionError("This should never execute.")

  implicit def makeIntellijHappy2[T<:raw.Node](x: scala.xml.Elem): T =
    throw new AssertionError("This should never execute.")

  def getElem[T](id: String): T = document.getElementById(id).asInstanceOf[T]
}
