package rlp.controllers

import com.thoughtworks.binding.Binding
import com.thoughtworks.binding.Binding.Constant
import org.scalajs.dom.raw.HTMLElement
import rlp._

trait ModelController[A] {

  val name: String

  lazy val modelBuilder: Binding[HTMLElement] = Binding { <div></div> }

  lazy val modelViewer: Binding[HTMLElement] = Binding { <div></div> }

  lazy val buildValid: Binding[Boolean] = Constant(true)

  lazy val agent: A = buildAgent()

  def buildAgent(): A

  def resetAgent(): Unit
}

object ModelController {
  type Builder[A] = (String, (() => ModelController[A]))
}

