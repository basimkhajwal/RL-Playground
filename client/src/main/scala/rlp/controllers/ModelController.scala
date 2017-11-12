package rlp.controllers

import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.html.Div

trait ModelController[A] {

  val name: String

  @dom
  lazy val modelOptions: Binding[Div] = {
    <div>Empty model options for { name }</div>
  }

  def buildAgent(): A
}
