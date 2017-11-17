package rlp

import org.scalajs.dom.raw.{HTMLElement, KeyboardEvent}

import scala.collection.mutable

class KeyboardHandler {

  val keyMap: mutable.Map[String, Boolean] = mutable.Map()

  def isKeyDown(key: String): Boolean = keyMap.getOrElse(key, false)

  def register(item: HTMLElement): Unit = {
    Logger.log("KeyHandler", "Registered item " + item.nodeName)
    item.addEventListener[KeyboardEvent]("keydown", { e:KeyboardEvent => println("KEY UP " + e.key) })
    item.onkeyup = { e:KeyboardEvent => keyMap.put(e.key, false) }
  }
}

