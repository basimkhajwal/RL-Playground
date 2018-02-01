package rlp.agent

import rlp.ai.ActivationFunction.{Linear, ReLU}
import rlp.ai.NeuralNetwork
import rlp.ai.optimizers.Adam

object BanditTest {

  def main(args: Array[String]): Unit = {

    val bandits = Array(
      -10000, 10, 12, -1
    )

    val qTable = new QTableAgent(1, bandits.length)
    val qNetwork = new QNetworkAgent(new NeuralNetwork(Array(1, 4, 4), Array(ReLU, Linear)), 1000)

    qTable.discountFactor = 0
    qTable.explorationEpsilon = 0.5

    qNetwork.explorationEpsilon = 0.5
    qNetwork.discountFactor = 0
    qNetwork.optimiser = new Adam(qNetwork.network, 0.01)
    qNetwork.miniBatchSize = 10
    qNetwork.updateStepInterval = 1
    qNetwork.network.initialiseWeights()

    for (_ <- 0 until 10000) {
      val r1 = bandits(qTable.act(0)) * (0.8 + math.random()*0.4)
      val r2 = bandits(qNetwork.act(Array(1.0))) * (0.8 + math.random()*0.4)
      qTable.percept(r1)
      qNetwork.percept(r2)
    }

    println(qTable.table mkString "\t")
    println(qNetwork.network.weights(0).toString)
    println(qNetwork.network.forwardProp(Array(1.0)) mkString "\t")
  }
}
