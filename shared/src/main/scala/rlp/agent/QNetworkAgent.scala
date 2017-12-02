package rlp.agent

import rlp.ai.NeuralNetwork
import rlp.ai.optimizers.{Adam, SGDMomentum}
import rlp.math.Matrix

class QNetworkAgent(
  val network: NeuralNetwork
) extends SteppedAgent[Array[Double], Int]{

  type Replay = (Array[Double], Int, Double, Array[Double])

  val replayBuffer = new Array[Replay](100)
  var replayBufferIdx = 0
  var isFull = false

  val optimiser = new SGDMomentum(network)
  val learningRate = 0.1
  val discountFactor = 0.99

  // TODO: Implement experience replay
  // TODO: Fix the issue when the environment is reset

  override def step(prevState: Array[Double], action: Int, reward: Double, newState: Array[Double], first: Boolean, last: Boolean): Int = {

    val newReturns = network.forwardProp(newState)
    var maxIdx = -1
    var maxAns = -1.0
    for (i <- newReturns.indices) {
      if (newReturns(i) > maxAns) {
        maxAns = newReturns(i)
        maxIdx = i
      }
    }

    if (prevState != null) {

      // Add replay to buffer
      replayBuffer(replayBufferIdx) = (prevState, action, reward, newState)
      replayBufferIdx += 1

      if (replayBufferIdx >= replayBuffer.length) {
        replayBufferIdx -= replayBuffer.length
        isFull = true
      }

      val returns = network.forwardProp(prevState)
      returns(action) += learningRate * (reward + maxAns * discountFactor - returns(action))

      optimiser.step(Matrix.rows(prevState), Matrix.rows(returns))
    }

    maxIdx
  }
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
