package rlp.utils

import org.scalajs.dom.raw.{HTMLElement, KeyboardEvent}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class KeyboardHandler {

  val captureKeys = List("ArrowUp", "ArrowDown", " ")

  val keyMap: mutable.Map[String, Boolean] = mutable.Map()
  val clickListeners: ListBuffer[String => _] = ListBuffer()

  def isKeyDown(key: String): Boolean = keyMap.getOrElse(key, false)

  def registerClickListener(listener: String => _): Unit = {
    clickListeners += listener
  }

  def register(element: HTMLElement): Unit = {
    element.onkeydown = { e:KeyboardEvent => keyMap.put(e.key, true); if (captureKeys contains e.key) e.preventDefault() }
    element.onkeyup = { e:KeyboardEvent =>
      keyMap.put(e.key, false)
      for (listener <- clickListeners) listener(e.key)

      if (captureKeys contains e.key) {
        e.preventDefault()
      }
    }
  }

}

