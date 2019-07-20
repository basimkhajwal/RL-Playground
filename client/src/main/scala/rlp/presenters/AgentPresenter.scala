package rlp.presenters

import com.thoughtworks.binding.{Binding, dom}
import com.thoughtworks.binding.Binding.{BindingSeq, Constant, Var}
import org.scalajs.dom.raw.HTMLElement
import rlp._
import rlp.utils.HistoryBuffer
import ujson.Js

/**
  * A class which represents how a particular agent in an environment
  * is represented in the user interface, it is combined of two parts:
  *
  * 1. agentBuilder - The interface for creating a new agent
  * 2. agentTrainer - The interface for interacting with and updating an existing agent
  *
  * @param environmentName The environment for this agent
  * @param agentName The type of agent
  * @tparam A
  */
abstract class AgentPresenter[A](val environmentName: String, val agentName: String) {


  private val _viewDirty: Var[Boolean] = Var(false)

  /**
    * A Binding to whether the view is dirty
    */
  val viewDirtyBinding: Binding[Boolean] = _viewDirty

  /** The view dirty property represents whether or not the changes in this
    * agents view have been reflected in the upstream UI or not.
    * */
  def viewDirty: Boolean = _viewDirty.get

  /**
    * A change that requires upstream UI changes
    */
  protected def viewChanged(): Unit = {
    _viewDirty := true
  }

  /**
    * The view has been updated and is no longer dirty (if it was)
    */
  def resetViewDirty(): Unit = {
    _viewDirty := false
  }

  private var _id: Long = 0

  /**
    * @return Unique identifier
    */
  def id: Long = _id

  def setId(newId: Long): Unit = {
    _id = newId
  }

  // The maximum history samples an agent presenter can store
  final val MAX_HISTORY = 2000

  val name: Var[String] = Var("")
  val gamesPlayed: Var[Int] = Var(0)

  private val historyBuffer = new HistoryBuffer(MAX_HISTORY)

  final val performanceStep: Binding[Int] = historyBuffer.historyStep
  final val performanceHistory: BindingSeq[Double] = historyBuffer.history

  /**
    * Add a new performance record to the buffer
    *
    * @param performance
    */
  def logPerformance(performance: Double): Unit = {
    historyBuffer.add(performance)
  }

  @dom
  lazy val agentBuilder: Binding[HTMLElement] = <div></div>

  @dom
  lazy val agentViewer: Binding[HTMLElement] = <div></div>

  /**
    * Whether the current build is valid (to allow creating the agent) or not
    */
  lazy val buildValid: Binding[Boolean] = Constant(true)

  /**
    * The agent itself, lazy so that it is only built when first accessed and only once.
    */
  lazy val agent: A = buildAgent()

  /**
    * Replicate the build to from another presenter
    * @param presenter
    */
  def cloneBuildFrom(presenter: AgentPresenter[A]): Unit

  /**
    * Use the build information to generate the agent
    *
    * @return
    */
  protected def buildAgent(): A

  /**
    * Serialise the build data
    * @return
    */
  protected def storeBuild(): Js.Value

  /**
    * Serialise the training data
    * @return
    */
  protected def storeAgent(): Js.Value

  /**
    * Load the build data from a serialised JSON object
    * @param build
    */
  protected def loadBuild(build: Js.Value): Unit

  /**
    * Load the agent data from a serialised JSON object
    * @param build
    */
  protected def loadAgent(build: Js.Value): Unit

  /**
    * Load the agent state from a store
    *
    * Note: agent MUST be un-built at this stage
    *
    * @param agentStore
    */
  def load(agentStore: AgentStore): Unit = {

    // Check for valid matching with this presenter type
    require(environmentName == agentStore.environmentName,
      s"Invalid environment, expected $environmentName but given ${agentStore.environmentName}")
    require(agentName == agentStore.agentName,
      s"Invalid agent, expected $agentName, but given ${agentStore.agentName}")

    // Update id, name and episode count
    setId(agentStore.id)
    name := agentStore.name
    gamesPlayed := agentStore.gamesPlayed

    // Reload history
    historyBuffer.clear()
    historyBuffer.history.get.appendAll(agentStore.performanceHistory)
    historyBuffer.historyStep := agentStore.performanceStep

    // Load build, build agent then load train preferences
    loadBuild(ujson.read(agentStore.buildData))
    agent
    loadAgent(ujson.read(agentStore.agentData))
  }

  /**
    * Store the agent state
    * @return
    */
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
      ujson.write(storeBuild()),
      ujson.write(storeAgent())
    )
  }

  /**
    * Reset the agent to its untrained state
    */
  def resetAgent(): Unit = {
    gamesPlayed := 0
    historyBuffer.clear()
  }

  override def toString: String = agentName + " - " + name.get
}

object AgentPresenter {

  /**
    * A Builder type represents a function which builds an
    * AgentPresenter given an agent type
    *
    * @tparam A
    */
  type Builder[A] = (String, (() => AgentPresenter[A]))
}