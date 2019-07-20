package rlp.presenters

import com.thoughtworks.binding.Binding.Var
import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.html.Div
import rlp.agent.{Agent, PolicyNetworkAgent}
import rlp.ai.NeuralNetwork
import rlp._
import rlp.agent.PolicyNetworkAgent.PolicyNetworkSpace
import rlp.ai.optimizers.NetworkOptimizer
import rlp.ui.NumericInputHandler
import ujson.Js

/**
  * The presenter for the PolicyNetworkAgent
  *
  * @param environment The environment for this agent presenter
  * @param numActions
  * @param actionMap
  * @param params The parameters for the agent
  * @tparam S The environment state type
  * @tparam A The action type
  */
class PolicyNetworkPresenter[S, A](
  environment: String,
  numActions: Int,
  actionMap: Int => A,
  params: Array[AgentParam[PolicyNetworkSpace[S]]]
) extends NetworkPresenter[S, A, PolicyNetworkSpace[S]](
  environment, PolicyNetworkPresenter.name,
  params, { p => p.size }, numActions
) {

  override val enableSoftMax: Boolean = true

  private var policyNetwork: PolicyNetworkAgent = _

  @dom
  override lazy val agentBuilder: Binding[Div] = {
    <div>
      {networkBuilder.bind}
    </div>
  }

  override def buildAgent(): Agent[S, A] = {
    val inputs = for ((param, enabled) <- paramBindings.get; if enabled) yield param.value
    val network = buildNetwork()

    policyNetwork = new PolicyNetworkAgent(network)
    policyNetwork.reset()

    discountFactor := policyNetwork.discountFactor

    PolicyNetworkAgent.build(policyNetwork, actionMap, inputs)
  }

  private val discountFactor: Var[Double] = Var(0.9)

  private def paramsChanged(discount: Double): Unit = {
    policyNetwork.discountFactor = discount

    viewChanged()
  }

  @dom
  override lazy val agentViewer: Binding[Div] = {
    <div>
      <div class="content-section">
        <h5>Policy Gradient Parameters</h5>
        <div class="divider"></div>

        <div class="row">
          <div class="col s3 offset-s2">
            { new NumericInputHandler("Discount Factor", discountFactor, 0, 1).content.bind }
          </div>
        </div>

      </div>

      { paramSelector.viewer.bind }
      { networkViewer.bind }

      {
        paramsChanged(discountFactor.bind)
        ""
      }

    </div>
  }

  override def load(agentStore: AgentStore): Unit = {
    super.load(agentStore)

    discountFactor := policyNetwork.discountFactor
  }

  override def resetAgent(): Unit = {
    super.resetAgent()
    policyNetwork.reset()
  }

  override protected def getNetwork(): NeuralNetwork = policyNetwork.network

  override protected def getOptimiser(): NetworkOptimizer = policyNetwork.optimiser

  override protected def setOptimiser(optimiser: NetworkOptimizer): Unit = {
    policyNetwork.optimiser = optimiser
  }

  override protected def storeAgent(): Js.Value = policyNetwork.store()

  override protected def loadAgent(data: Js.Value): Unit = policyNetwork.load(data)
}

object PolicyNetworkPresenter {

  val name = "Policy Network"

  def builder[S,A](
    environment: String,
    numActions: Int, actionMap: Int => A,
    params: AgentParam[PolicyNetworkSpace[S]]*
  ): AgentPresenter.Builder[Agent[S,A]] = {

    name -> (() => new PolicyNetworkPresenter(environment, numActions, actionMap, params.toArray))
  }
}
