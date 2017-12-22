package rlp.agent

import rlp.ai.NeuralNetwork
import rlp.ai.optimizers.{Adam, NetworkOptimizer, SGDMomentum}
import rlp.math.Matrix
import upickle.Js

class QNetworkAgent(
  val network: NeuralNetwork
) extends SteppedAgent[Array[Double], Int]{

  type Replay = (Array[Double], Int, Double, Array[Double])

  val replayBuffer = new Array[Replay](100)
  var replayBufferIdx = 0
  var isFull = false

  var optimiser: NetworkOptimizer = new SGDMomentum(network, 0.0001)
  val discountFactor = 0.99

  // TODO: Implement experience replay

  override def step(prevState: Array[Double], action: Int, reward: Double, newState: Array[Double], first: Boolean, last: Boolean): Int = {

    val newReturns = network.forwardProp(newState)
    var maxIdx = 0
    var maxAns = Double.NegativeInfinity
    for (i <- newReturns.indices) {
      if (newReturns(i) > maxAns) {
        maxAns = newReturns(i)
        maxIdx = i
      }
    }

    if (!first) {

      val returns = network.forwardProp(prevState)
      returns(action) = reward + (if (last) 0 else maxAns) * discountFactor

      optimiser.step(Matrix.rows(prevState), Matrix.rows(returns))
    }

    maxIdx
  }

  override def reset(): Unit = {
    network.initialiseWeights()
  }

  override def load(data: Js.Value): Unit = ???

  override def store(): Js.Value = ???
}

object QNetworkAgent {

  case class QNetworkSpace[T](size: Int, map: T => Array[Double]) {
    def apply(state: T) = map(state)
  }

  def stateMap[S](spaces: Array[QNetworkSpace[S]])(state: S): Array[Double] = {
    spaces.flatMap(_(state))
  }

  def build[S,A](qAgent: QNetworkAgent, actionMap: Int => A, spaces: Seq[QNetworkSpace[S]]): Agent[S,A] = {
    new MappedAgent(qAgent, stateMap(spaces.toArray), actionMap)
  }
}
