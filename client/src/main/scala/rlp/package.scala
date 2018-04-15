import com.thoughtworks.binding.Binding
import com.thoughtworks.binding.dom.Runtime.TagsAndTags2
import org.scalajs.dom.raw
import org.scalajs.dom.document

import scala.collection.mutable
import scala.scalajs.js
import scalatags.JsDom

/**
  * Various commonly used functions, mainly to access HTML5 elements
  * or type hacks to satisfy the compiler
  */
package object rlp {

  implicit def makeIntellijHappy[T<:raw.Node](x: scala.xml.Node): Binding[T] =
    throw new AssertionError("This should never execute.")

  implicit def makeIntellijHappy2[T<:raw.Node](x: scala.xml.Elem): T =
    throw new AssertionError("This should never execute.")

  implicit def toSvgTags(x: TagsAndTags2.type) = JsDom.svgTags

  // Keep track of GUIDs and assign them to uniquely add ID's for div elements in the page

  private val counters = mutable.Map[String,Int]()

  def getGUID(prefix: String = ""): String = {
    counters(prefix) = counters.getOrElse(prefix, 0) + 1
    prefix + counters(prefix)
  }

  /**
    * Run initialisation scripts on the element when it is added to the DOM
    *
    * @param id
    * @param initialTimeout
    * @param refreshInterval
    * @param init
    */
  def initScript(id: String, initialTimeout: Double = 0, refreshInterval: Double = 50)(init: () => Unit): Unit = {
    var refreshTimer: js.timers.SetIntervalHandle = null

    def checkInit(): Unit = {
      if (document.getElementById(id) != null) {
        js.timers.clearInterval(refreshTimer)
        init()
      }
    }

    def startTimer(): Unit = {
      refreshTimer = js.timers.setInterval(refreshInterval) { checkInit() }
    }

    js.timers.setTimeout(initialTimeout) { startTimer() }
  }

  def initModal(id: String, opts: js.Any = js.Dynamic.literal()): Unit = {
    initScript(id) { () =>
      js.Dynamic.global.$("#" + id).modal(opts)
    }
  }

  /* Jquery handlers */

  def getElem[T](id: String): T = document.getElementById(id).asInstanceOf[T]

  def querySelector[T](query: String): T = document.querySelector(query).asInstanceOf[T]
}
