package rlp.controllers

import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.html.Div
import rlp.environment.Agent


trait ModelController[S, O, A] {

  val name: String

  @dom
  lazy val options: Binding[Div] = { <div>Empty model options for { name }</div> }

  def getTrainedAgent(): Agent[O, A]

  def getTrainingState(): S
  
  def trainStep(): Unit
}
