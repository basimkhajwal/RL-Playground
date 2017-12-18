import com.thoughtworks.binding.Binding
import com.thoughtworks.binding.dom.Runtime.TagsAndTags2
import org.scalajs.dom.raw
import org.scalajs.dom.document

import scala.collection.mutable
import scalatags.JsDom

package object rlp {

  implicit def makeIntellijHappy[T<:raw.Node](x: scala.xml.Node): Binding[T] =
    throw new AssertionError("This should never execute.")

  implicit def makeIntellijHappy2[T<:raw.Node](x: scala.xml.Elem): T =
    throw new AssertionError("This should never execute.")

  implicit def toSvgTags(x: TagsAndTags2.type) = JsDom.svgTags

  private val counters = mutable.Map[String,Int]()

  def getGUID(prefix: String = ""): String = {
    counters(prefix) = counters.getOrElse(prefix, 0) + 1
    prefix + counters(prefix)
  }

  def getElem[T](id: String): T = document.getElementById(id).asInstanceOf[T]

  def querySelector[T](query: String): T = document.querySelector(query).asInstanceOf[T]
}
