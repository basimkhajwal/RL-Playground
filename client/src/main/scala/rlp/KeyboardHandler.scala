package rlp

import org.scalajs.dom.raw.{HTMLElement, KeyboardEvent}
import org.scalajs.dom.document

import scala.collection.mutable

class KeyboardHandler {

  val captureKeys = List("ArrowUp", "ArrowDown")

  val keyMap: mutable.Map[String, Boolean] = mutable.Map()

  def isKeyDown(key: String): Boolean = keyMap.getOrElse(key, false)

  def register(element: HTMLElement): Unit = {
    element.onkeydown = { e:KeyboardEvent => keyMap.put(e.key, true); if (captureKeys contains e.key) e.preventDefault() }
    element.onkeyup = { e:KeyboardEvent => keyMap.put(e.key, false); if (captureKeys contains e.key) e.preventDefault() }
  }

}

