package rlp.models

import com.thoughtworks.binding.{Binding, dom}
import com.thoughtworks.binding.Binding.{BindingSeq, Constant, Var}
import org.scalajs.dom.raw.HTMLElement
import rlp._
import rlp.utils.HistoryBuffer
import upickle.{Js, json}

abstract class Model[A](val environmentName: String, val agentName: String) {

  private val _viewDirty: Var[Boolean] = Var(false)

  val viewDirtyBinding: Binding[Boolean] = _viewDirty
  def viewDirty: Boolean = _viewDirty.get

  protected def viewChanged(): Unit = {
    _viewDirty := true
  }

  def resetViewDirty(): Unit = {
    _viewDirty := false
  }

  private var _id: Long = 0

  def id: Long = _id

  def setId(newId: Long): Unit = {
    _id = newId
  }

  final val MAX_HISTORY = 2000

  val modelName: Var[String] = Var("")
  val gamesPlayed: Var[Int] = Var(0)

  private val historyBuffer = new HistoryBuffer(MAX_HISTORY)

  final val performanceStep: Binding[Int] = historyBuffer.historyStep
  final val performanceHistory: BindingSeq[Double] = historyBuffer.history

  def logPerformance(performance: Double): Unit = {
    historyBuffer.add(performance)
  }

  @dom
  lazy val modelBuilder: Binding[HTMLElement] = <div></div>

  @dom
  lazy val modelViewer: Binding[HTMLElement] = <div></div>

  lazy val buildValid: Binding[Boolean] = Constant(true)

  lazy val agent: A = buildAgent()

  def cloneBuildFrom(controller: Model[A]): Unit

  protected def buildAgent(): A

  protected def storeBuild(): Js.Value

  protected def storeAgent(): Js.Value

  protected def loadBuild(build: Js.Value): Unit

  protected def loadAgent(build: Js.Value): Unit

  def load(modelStore: ModelStore): Unit = {
    require(environmentName == modelStore.environmentName,
      s"Invalid environment, expected $environmentName but given ${modelStore.environmentName}")
    require(agentName == modelStore.agentName,
      s"Invalid agent, expected $agentName, but given ${modelStore.agentName}")

    setId(modelStore.id)

    modelName := modelStore.modelName
    gamesPlayed := modelStore.gamesPlayed

    historyBuffer.clear()
    historyBuffer.history.get.appendAll(modelStore.performanceHistory)
    historyBuffer.historyStep := modelStore.performanceStep

    loadBuild(json.read(modelStore.buildData))
    agent
    loadAgent(json.read(modelStore.agentData))
  }

  final def store(): ModelStore = {
    ModelStore(
      id,
      System.currentTimeMillis(),
      environmentName,
      agentName,
      modelName.get,
      gamesPlayed.get,
      historyBuffer.historyStep.get,
      historyBuffer.history.get,
      json.write(storeBuild()),
      json.write(storeAgent())
    )
  }

  def resetAgent(): Unit = {
    gamesPlayed := 0
    historyBuffer.clear()
  }

  override def toString: String = agentName + " - " + modelName.get
}

object Model {
  type Builder[A] = (String, (() => Model[A]))
}