package rlp.agent

import rlp.ai.NeuralNetwork

class QNetworkAgent(
  val network: NeuralNetwork
) extends SteppedAgent[Array[Double], Array[Double]]{

  override def step(prevState: Array[Double], action: Array[Double], reward: Double, newState: Array[Double]): Array[Double] = {
    ???
  }
}
