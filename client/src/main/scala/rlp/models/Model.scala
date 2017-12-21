package rlp.models

import com.thoughtworks.binding.{Binding, dom}
import com.thoughtworks.binding.Binding.{Constant, Var, Vars}
import org.scalajs.dom.raw.HTMLElement
import org.scalajs.dom.window.performance
import rlp._

abstract class Model[A](
  val environmentName: String,
  val agentName: String,
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

  protected def serializeBuild(): String

  protected def serializeAgent(): String

  final def store(environmentName: String): ModelStore = {
    ModelStore(
      environmentName,
      agentName,
      modelName.get,
      gamesPlayed.get,
      performanceHistory.get,
      serializeBuild(),
      serializeAgent()
    )
  }

  def resetAgent(): Unit = {
    gamesPlayed := 0
    performanceHistory.get.clear()
  }

  override def toString: String = agentName + " - " + modelName.get
}

object Model {
  type Builder[A] = (String, (() => Model[A]))
}

