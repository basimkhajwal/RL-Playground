package rlp

import org.scalajs.dom.raw.{HTMLElement, KeyboardEvent}
import org.scalajs.dom.document

import scala.collection.mutable

class KeyboardHandler {

  val keyMap: mutable.Map[String, Boolean] = mutable.Map()

  def isKeyDown(key: String): Boolean = keyMap.getOrElse(key, false)

  document.onkeydown = { e:KeyboardEvent => keyMap.put(e.key, true) }
  document.onkeyup = { e:KeyboardEvent => keyMap.put(e.key, false) }
}

