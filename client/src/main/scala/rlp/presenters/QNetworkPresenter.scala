package rlp.presenters

import com.thoughtworks.binding.Binding.Var
import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.html
import rlp.agent.QNetworkAgent.QNetworkSpace
import rlp.agent.{Agent, QNetworkAgent}
import rlp.ai.NeuralNetwork
import rlp._
import rlp.ai.optimizers.NetworkOptimizer
import rlp.ui.NumericInputHandler
import ujson.Js

/**
  * The presenter for the QNetworkAgent
  *
  * @param environment The environment for this agent presenter
  * @param numActions
  * @param actionMap Map indexes (0 .. numActions-1) to an action type
  * @param params The parameters for the agent
  * @tparam S The environment state type
  * @tparam A The action type
  */
class QNetworkPresenter[S,A](
  environment: String,
  numActions: Int,
  actionMap: Int => A,
  params: Array[AgentParam[QNetworkSpace[S]]]
) extends NetworkPresenter[S, A, QNetworkSpace[S]](
  environment, QNetworkPresenter.name,
  params,
  { p => p.size }, numActions
) {

  private var qNetwork: QNetworkAgent = _

  // The only extra build time parameter
  private val replayBufferSize: Var[Int] = Var(1000)

  @dom
  override lazy val agentBuilder: Binding[html.Div] = {
    <div>
      <div class="content-section">
        <h5>Experience Replay</h5>
        <div class="divider"></div>

        <div class="row">
          <div class="col s4 offset-s4">
            { new NumericInputHandler("Replay Buffer Size", replayBufferSize, 1, 10000).content.bind }
          </div>
        </div>
      </div>

      {networkBuilder.bind}
    </div>
  }

  override def buildAgent(): Agent[S, A] = {
    val inputs = for ((param, enabled) <- paramBindings.get; if enabled) yield param.value
    val network = buildNetwork()

    qNetwork = new QNetworkAgent(network, replayBufferSize.get)
    qNetwork.reset()

    explorationEpsilon := qNetwork.explorationEpsilon
    discountFactor := qNetwork.discountFactor
    miniBatchSize := qNetwork.miniBatchSize
    updateStepInterval := qNetwork.updateStepInterval

    QNetworkAgent.build(qNetwork, actionMap, inputs)
  }

  override def cloneBuildFrom(that: AgentPresenter[Agent[S, A]]): Unit = {
    super.cloneBuildFrom(that)

    val controller = that.asInstanceOf[QNetworkPresenter[S, A]]
    replayBufferSize := controller.replayBufferSize.get
  }

  override def storeBuild(): Js.Value = {
    val networkStore = super.storeBuild()

    Js.Obj(
      "networkStore" -> networkStore,
      "replayBufferSize" -> Js.Num(replayBufferSize.get)
    )
  }

  override def loadBuild(build: Js.Value): Unit = {
    val keyMap = build.obj

    super.loadBuild(keyMap("networkStore"))
    replayBufferSize := keyMap("replayBufferSize").num.toInt
  }

  // Train time hyper-parameters
  private val explorationEpsilon: Var[Double] = Var(0.1)
  private val discountFactor: Var[Double] = Var(0.99)
  private val miniBatchSize: Var[Int] = Var(10)
  private val updateStepInterval: Var[Int] = Var(50)

  /**
    * When any parameter changes update the qNetwork
    *
    * @param epsilon
    * @param discount
    * @param batchSize
    * @param updateInterval
    */
  private def paramsChanged(epsilon: Double, discount: Double, batchSize: Int, updateInterval: Int): Unit = {
    qNetwork.explorationEpsilon = epsilon
    qNetwork.discountFactor = discount
    qNetwork.miniBatchSize = batchSize
    qNetwork.updateStepInterval = updateInterval

    viewChanged()
  }

  @dom
  override lazy val agentViewer: Binding[html.Div] = {
    <div>

      <div class="content-section">
        <h5>Q Learning Parameters</h5>
        <div class="divider"></div>

        <div class="row">
          <div class="col s3 offset-s2">
            { new NumericInputHandler("Exploration Epsilon", explorationEpsilon, 0, 1).content.bind }
          </div>
          <div class="col s3 offset-s2">
            { new NumericInputHandler("Discount Factor", discountFactor, 0, 1).content.bind }
          </div>
        </div>

        <h5>Experience Replay</h5>
        <div class="divider"></div>

        <div class="row">
          <div class="col s2 offset-s2">
            <div class="input-field">
              <input type="number" disabled={true} value={qNetwork.replayBufferSize.toString}/>
              <label for="buffer-size-view" class="active">Buffer Size</label>
            </div>
          </div>

          <div class="col s2 offset-s1">
            { new NumericInputHandler("Mini-batch Size", miniBatchSize, 1, qNetwork.replayBufferSize).content.bind }
          </div>

          <div class="col s2 offset-s1">
            { new NumericInputHandler("Update Step Interval", updateStepInterval, 1, 10000).content.bind }
          </div>
        </div>

      </div>

      { paramSelector.viewer.bind }

      { networkViewer.bind }

      {
        paramsChanged(explorationEpsilon.bind, discountFactor.bind, miniBatchSize.bind, updateStepInterval.bind)
        ""
      }
    </div>
  }

  override def load(agentStore: AgentStore): Unit = {
    super.load(agentStore)

    explorationEpsilon := qNetwork.explorationEpsilon
    discountFactor := qNetwork.discountFactor
    miniBatchSize := qNetwork.miniBatchSize
    updateStepInterval := qNetwork.updateStepInterval
  }

  override def resetAgent(): Unit = {
    super.resetAgent()
    qNetwork.reset()
  }

  override protected def getNetwork(): NeuralNetwork = qNetwork.network

  override protected def getOptimiser(): NetworkOptimizer = qNetwork.optimiser

  override protected def setOptimiser(optimiser: NetworkOptimizer): Unit = {
    qNetwork.optimiser = optimiser
  }

  override protected def storeAgent(): Js.Value = qNetwork.store()

  override protected def loadAgent(data: Js.Value): Unit = qNetwork.load(data)
}

object QNetworkPresenter {

  val name = "Q Network"

  def builder[S,A](
    environment: String,
    numActions: Int, actionMap: Int => A,
    params: AgentParam[QNetworkSpace[S]]*
  ): AgentPresenter.Builder[Agent[S,A]] = {

    name -> (() => new QNetworkPresenter(environment, numActions, actionMap, params.toArray))
  }
}
