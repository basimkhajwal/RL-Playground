package rlp.models

import com.thoughtworks.binding.{Binding, dom}
import com.thoughtworks.binding.Binding.{Constant, Var, Vars}
import org.scalajs.dom.raw.HTMLElement
import org.scalajs.dom.window.performance
import rlp._

abstract class Model[A](
  val controllerName: String,
  val id: Long = (1000 * performance.now()).toLong
) {

  val modelName: Var[String] = Var("")
  val gamesPlayed: Var[Int] = Var(0)

  val performanceHistory: Vars[Double] = Vars()

  @dom
  lazy val modelBuilder: Binding[HTMLElement] = <div></div>

  @dom
  lazy val modelViewer: Binding[HTMLElement] = <div></div>

  lazy val buildValid: Binding[Boolean] = Constant(true)

  lazy val agent: A = buildAgent()

  def cloneBuildFrom(controller: Model[A]): Unit

  def buildAgent(): A

  def resetAgent(): Unit = {
    gamesPlayed := 0
    performanceHistory.get.clear()
  }

  override def toString: String = controllerName + " - " + modelName.get
}

object Model {
  type Builder[A] = (String, (() => Model[A]))
}

