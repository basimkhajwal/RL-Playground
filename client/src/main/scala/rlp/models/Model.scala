package rlp.models

import com.thoughtworks.binding.{Binding, dom}
import com.thoughtworks.binding.Binding.{BindingSeq, Constant, Var, Vars}
import org.scalajs.dom.raw.HTMLElement
import org.scalajs.dom.window.performance
import rlp._
import rlp.storage.ModelStore
import upickle.{Js, json}

import scala.collection.mutable.ArrayBuffer

abstract class Model[A](
  val environmentName: String,
  val agentName: String,
  val id: Long = (1000 * performance.now()).toLong
) {

  final val MAX_HISTORY = 2000

  val modelName: Var[String] = Var("")
  val gamesPlayed: Var[Int] = Var(0)

  private val _viewDirty: Var[Boolean] = Var(false)

  val viewDirtyBinding: Binding[Boolean] = _viewDirty
  def viewDirty: Boolean = _viewDirty.get

  protected def viewChanged(): Unit = {
    _viewDirty := true
  }

  def resetViewDirty(): Unit = {
    _viewDirty := false
  }

  private val historyStep: Var[Int] = Var(1)
  private val history: Vars[Double] = Vars()
  private var recentHistoryTotal: Double = 0
  private var recentHistoryCount: Int = 0

  final val performanceStep: Binding[Int] = historyStep
  final val performanceHistory: BindingSeq[Double] = history

  final def clearHistory(): Unit = {
    historyStep := 1
    history.get.clear()
    recentHistoryCount = 0
    recentHistoryTotal = 0
  }

  private final def collateHistory(): Unit = {
    val currentHistory = history.get
    val newHistory = ArrayBuffer[Double]()

    newHistory += currentHistory(0)
    for (i <- 2 until currentHistory.length by 2) {
      newHistory += (currentHistory(i) + currentHistory(i-1)) / 2
    }

    historyStep := historyStep.get * 2
    history.get.clear()
    history.get.appendAll(newHistory)
  }

  final def logPerformance(performance: Double): Unit = {
    recentHistoryTotal += performance
    recentHistoryCount += 1

    if (recentHistoryCount == historyStep.get) {
      history.get += recentHistoryTotal / historyStep.get

      recentHistoryTotal = 0
      recentHistoryCount = 0

      if (history.get.length >= MAX_HISTORY) {
        collateHistory()
      }
    }
  }

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
    require(environmentName == modelStore.environmentName,
      s"Invalid environment, expected $environmentName but given ${modelStore.environmentName}")
    require(agentName == modelStore.agentName,
      s"Invalid agent, expected $agentName, but given ${modelStore.agentName}")

    modelName := modelStore.modelName
    gamesPlayed := modelStore.gamesPlayed

    clearHistory()
    history.get.appendAll(modelStore.performanceHistory)
    historyStep := modelStore.performanceStep

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
      historyStep.get,
      history.get,
      json.write(storeBuild()),
      json.write(storeAgent())
    )
  }

  def resetAgent(): Unit = {
    gamesPlayed := 0
    clearHistory()
  }

  override def toString: String = agentName + " - " + modelName.get
}

object Model {
  type Builder[A] = (String, (() => Model[A]))
}