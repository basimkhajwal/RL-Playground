package rlp.presenters

import com.thoughtworks.binding.{Binding, dom}
import com.thoughtworks.binding.Binding.{BindingSeq, Constant, Var}
import org.scalajs.dom.raw.HTMLElement
import rlp._
import rlp.utils.HistoryBuffer
import upickle.{Js, json}

abstract class AgentPresenter[A](val environmentName: String, val agentName: String) {

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

  val name: Var[String] = Var("")
  val gamesPlayed: Var[Int] = Var(0)

  private val historyBuffer = new HistoryBuffer(MAX_HISTORY)

  final val performanceStep: Binding[Int] = historyBuffer.historyStep
  final val performanceHistory: BindingSeq[Double] = historyBuffer.history

  def logPerformance(performance: Double): Unit = {
    historyBuffer.add(performance)
  }

  @dom
  lazy val agentBuilder: Binding[HTMLElement] = <div></div>

  @dom
  lazy val agentViewer: Binding[HTMLElement] = <div></div>

  lazy val buildValid: Binding[Boolean] = Constant(true)

  lazy val agent: A = buildAgent()

  def cloneBuildFrom(controller: AgentPresenter[A]): Unit

  protected def buildAgent(): A

  protected def storeBuild(): Js.Value

  protected def storeAgent(): Js.Value

  protected def loadBuild(build: Js.Value): Unit

  protected def loadAgent(build: Js.Value): Unit

  def load(agentStore: AgentStore): Unit = {
    require(environmentName == agentStore.environmentName,
      s"Invalid environment, expected $environmentName but given ${agentStore.environmentName}")
    require(agentName == agentStore.agentName,
      s"Invalid agent, expected $agentName, but given ${agentStore.agentName}")

    setId(agentStore.id)

    name := agentStore.name
    gamesPlayed := agentStore.gamesPlayed

    historyBuffer.clear()
    historyBuffer.history.get.appendAll(agentStore.performanceHistory)
    historyBuffer.historyStep := agentStore.performanceStep

    loadBuild(json.read(agentStore.buildData))
    agent
    loadAgent(json.read(agentStore.agentData))
  }

  final def store(): AgentStore = {
    AgentStore(
      id,
      System.currentTimeMillis(),
      environmentName,
      agentName,
      name.get,
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

  override def toString: String = agentName + " - " + name.get
}

object AgentPresenter {
  type Builder[A] = (String, (() => AgentPresenter[A]))
}