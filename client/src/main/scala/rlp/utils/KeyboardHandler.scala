package rlp.utils

import org.scalajs.dom.raw.{HTMLElement, KeyboardEvent}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Utility to keep track of the keyboard state
  */
class KeyboardHandler {

  // Keys to capture and avoid default behaviour (e.g. page navigation)
  val captureKeys = List("ArrowUp", "ArrowDown", " ")

  // Buffer for which keys have been pressed
  val keyMap: mutable.Map[String, Boolean] = mutable.Map()

  // Listeners to key events
  val clickListeners: ListBuffer[String => _] = ListBuffer()

  def isKeyDown(key: String): Boolean = keyMap.getOrElse(key, false)

  def registerClickListener(listener: String => _): Unit = {
    clickListeners += listener
  }

  /**
    * Register this handler to an element which captures
    * necessary keys and attaches listeners
    *
    * @param element
    */
  def register(element: HTMLElement): Unit = {

    element.onkeydown = { e:KeyboardEvent =>
      keyMap.put(e.key, true)

      if (captureKeys contains e.key) {
        e.preventDefault()
      }
    }

    element.onkeyup = { e:KeyboardEvent =>
      keyMap.put(e.key, false)

      for (listener <- clickListeners) {
        listener(e.key)
      }

      if (captureKeys contains e.key) {
        e.preventDefault()
      }
    }
  }

}

