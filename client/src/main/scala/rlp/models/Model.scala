package rlp.models

import com.thoughtworks.binding.{Binding, dom}
import com.thoughtworks.binding.Binding.{Constant, Var, Vars}
import org.scalajs.dom.raw.HTMLElement
import org.scalajs.dom.window.performance
import rlp._
import upickle.{Js, json}

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

  protected def storeBuild(): Js.Value

  protected def storeAgent(): Js.Value

  protected def loadBuild(build: Js.Value): Unit

  protected def loadAgent(build: Js.Value): Unit

  def load(modelStore: ModelStore): Unit = {
    require(environmentName == modelStore.environmentName && agentName == modelStore.agentName,
      "Invalid store type")

    modelName := modelStore.modelName
    gamesPlayed := modelStore.gamesPlayed

    performanceHistory.get.clear()
    performanceHistory.get.appendAll(modelStore.performanceHistory)

    loadBuild(json.read(modelStore.buildData))
    agent
    loadAgent(json.read(modelStore.agentData))
  }

  final def store(): ModelStore = {
    ModelStore(
      environmentName,
      agentName,
      modelName.get,
      gamesPlayed.get,
      performanceHistory.get,
      json.write(storeBuild()),
      json.write(storeAgent())
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

