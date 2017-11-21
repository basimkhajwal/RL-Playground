package rlp.controllers

import com.thoughtworks.binding.Binding
import com.thoughtworks.binding.Binding.Var
import org.scalajs.dom.raw.HTMLElement
import rlp._

trait ModelController[A] {

  val name: String

  lazy val modelBuilder: Binding[HTMLElement] = Binding { <div></div> }

  lazy val modelViewer: Binding[HTMLElement] = Binding { <div></div> }

  lazy val buildValid: Binding[Boolean] = Var(true)

  def buildModel(): A

}

object ModelController {
  type Builder[A] = (String, (() => ModelController[A]))
}

