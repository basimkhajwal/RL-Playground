package rlp.presenters

import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.html
import org.scalajs.dom.html.Div
import rlp.agent.{Agent, PolicyNetworkAgent}
import rlp.ai.NeuralNetwork
import rlp._
import rlp.agent.PolicyNetworkAgent.PolicyNetworkSpace
import rlp.ai.optimizers.NetworkOptimizer
import upickle.Js

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

    PolicyNetworkAgent.build(policyNetwork, actionMap, inputs)
  }

  @dom
  override lazy val agentViewer: Binding[Div] = {
    <div>
      <div class="content-section">
        <h5>Policy Gradient Parameters</h5>
        <div class="divider"></div>

        <div class="row">
          <h5>TODO....</h5>
        </div>

      </div>{paramSelector.viewer.bind}{networkViewer.bind}

    </div>
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
    params: AgentParam[PolicyNetworkSpace[S]]*): AgentPresenter.Builder[Agent[S,A]] = {

    name -> (() => new PolicyNetworkPresenter(environment, numActions, actionMap, params.toArray))
  }
}
