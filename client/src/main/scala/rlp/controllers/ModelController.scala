package rlp.controllers

import com.thoughtworks.binding.Binding
import com.thoughtworks.binding.Binding.{Constant, Var}
import org.scalajs.dom.raw.HTMLElement
import org.scalajs.dom.window.performance
import rlp._

abstract class ModelController[A](
                                 val controllerName: String,
                                 val id: Long = (1000 * performance.now()).toLong
                                 ) {

  val modelName: Var[String] = Var("")
  val gamesPlayed: Var[Int] = Var(0)

  lazy val modelBuilder: Binding[HTMLElement] = Binding { <div></div> }

  lazy val modelViewer: Binding[HTMLElement] = Binding { <div></div> }

  lazy val buildValid: Binding[Boolean] = Constant(true)

  lazy val agent: A = buildAgent()

  def cloneBuild(): ModelController[A]

  protected def _duplicate(): ModelController[A]

  final def duplicate(): ModelController[A] = {
    val newModel = _duplicate()

    newModel.modelName := modelName.get + " copy"
    newModel.gamesPlayed := gamesPlayed.get

    newModel
  }

  def buildAgent(): A

  def resetAgent(): Unit

  override def toString: String = controllerName + " - " + modelName.get
}

object ModelController {
  type Builder[A] = (String, (() => ModelController[A]))
}

