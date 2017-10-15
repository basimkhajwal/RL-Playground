import com.thoughtworks.binding.Binding
import org.scalajs.dom.raw

package object rlp {

  implicit def makeIntellijHappy[T<:raw.Node](x: scala.xml.Node): Binding[T] =
    throw new AssertionError("This should never execute.")

  implicit def makeIntellijHappy2[T<:raw.Node](x: scala.xml.Elem): T =
    throw new AssertionError("This should never execute.")
}
