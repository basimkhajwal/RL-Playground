package rlp.agent

import rlp.ai.NeuralNetwork
import rlp.ai.optimizers.{NetworkOptimizer, SGDMomentum}
import rlp.math.Matrix
import upickle.Js

import scala.collection.mutable.ArrayBuffer

class PolicyNetworkAgent(
  val network: NeuralNetwork
) extends Agent[Array[Double], Int] {

  val numActions = network.layerSizes(network.numLayers - 1)

  var discountFactor = 0.9

  var optimiser: NetworkOptimizer = new SGDMomentum(network)

  val episodeRewards = ArrayBuffer[Double]()
  val episodeGradients = ArrayBuffer[Array[Matrix]]()

  override def act(state: Array[Double]): Int = {
    val actionProbabilities = network.forwardProp(state)

    val sample = math.random()
    var probTotal = actionProbabilities(0)
    var action = 0

    while (probTotal < sample) {
      action += 1
      probTotal += actionProbabilities(action)
    }

    val oneHot = new Array[Double](numActions)
    oneHot(action) = 1.0

    episodeGradients += network.backProp(Matrix.rows(state), Matrix.rows(oneHot))

    action
  }

  override def percept(reward: Double): Unit = {
    episodeRewards += reward
  }

  private def computeReturns(): Array[Double] = {
    var totalReturn = 0.0
    val returns = new Array[Double](episodeRewards.length)

    for (i <- returns.indices.reverse) {
      totalReturn = episodeRewards(i) + discountFactor * totalReturn
      returns(i) = totalReturn
    }

    returns
  }

  override def resetEpisode(): Unit = {

    val returns = computeReturns()

    for (i <- returns.indices) {
      if (returns(i).abs > 1e-10) {
        for (grad <- episodeGradients(i)) {
          grad *= returns(i)
        }
        optimiser.step(episodeGradients(i))
      }
    }

    episodeRewards.clear()
    episodeGradients.clear()
  }

  override def clone(): Agent[Array[Double], Int] = {
    val cloned = new PolicyNetworkAgent(network)
    cloned.episodeRewards ++= episodeRewards
    cloned.episodeGradients ++= episodeGradients
    cloned
  }

  override def duplicate(): Agent[Array[Double], Int] = {
    val duplicate = new PolicyNetworkAgent(network.clone(true))
    duplicate.episodeRewards ++= episodeRewards
    duplicate.episodeGradients ++= episodeGradients
    duplicate
  }

  override def reset(): Unit = {
    network.initialiseWeights()
  }

  // TEMP, add parameters later

  override def store(): Js.Value = {
    network.store()
  }

  override def load(data: Js.Value): Unit = {
    network.load(data)
  }
}

object PolicyNetworkAgent {

  case class PolicyNetworkSpace[T](size: Int, map: T => Array[Double]) {
    def apply(state: T) = map(state)
  }

  object PolicyNetworkSpace {
    def bounded[T](min: Double, max: Double, map: T => Double): PolicyNetworkSpace[T] = {
      PolicyNetworkSpace(1, s => Array((map(s) - min) / (max - min)))
    }
  }

  def stateMap[S](spaces: Array[PolicyNetworkSpace[S]])(state: S): Array[Double] = {
    spaces.flatMap(_(state))
  }

  def build[S,A](policyAgent: PolicyNetworkAgent, actionMap: Int => A, spaces: Seq[PolicyNetworkSpace[S]]): Agent[S,A] = {
    new MappedAgent(policyAgent, stateMap(spaces.toArray), actionMap)
  }
}