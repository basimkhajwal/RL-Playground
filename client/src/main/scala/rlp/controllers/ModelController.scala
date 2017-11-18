package rlp.controllers

import com.thoughtworks.binding.Binding
import org.scalajs.dom.html.Div

trait ModelController[A] {

  val name: String

  def modelOptions(buildEnabled: Binding[Boolean]): Binding[Div]

  def validate(): Boolean = true

  def buildAgent(): A
}
