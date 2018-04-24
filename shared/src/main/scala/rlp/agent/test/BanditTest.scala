package rlp.agent.test

import rlp.agent.{PolicyNetworkAgent, QNetworkAgent, QTableAgent}
import rlp.ai.ActivationFunction.{Linear, ReLU}
import rlp.ai.NeuralNetwork
import rlp.ai.optimizers.Adam

import scala.util.Random

/**
  * Integration test for the Bandit problem
  */
object BanditTest {

  val random = new Random()

  // Gaussian deviation
  def gaussian(mean: Double = 0, stdDev: Double = 3): Double = {
    mean + stdDev * random.nextGaussian()
  }

  def main(args: Array[String]): Unit = {

    val bandits = Array(-10, 26, 12, 20)

    val qTable = new QTableAgent(1, bandits.length)
    val qNetwork = new QNetworkAgent(new NeuralNetwork(Array(1, 4, 4), Array(ReLU, Linear)), 1000)
    val policyNetwork = new PolicyNetworkAgent(new NeuralNetwork(Array(1, 4), Array(Linear), true))

    qTable.discountFactor = 0
    qTable.explorationEpsilon = 0.5

    policyNetwork.discountFactor = 0
    policyNetwork.network.initialiseWeights()

    qNetwork.explorationEpsilon = 0.5
    qNetwork.discountFactor = 0
    qNetwork.optimiser = new Adam(qNetwork.network, 0.01)
    qNetwork.miniBatchSize = 10
    qNetwork.updateStepInterval = 1
    qNetwork.network.initialiseWeights()

    for (_ <- 0 until 10000) {
      // Rewards for each agent
      val r1 = bandits(qTable.act(0))
      val r2 = bandits(qNetwork.act(Array(1.0)))
      val r3 = bandits(policyNetwork.act(Array(1.0)))

      // Percept these rewards
      qTable.percept(gaussian(r1))
      qNetwork.percept(gaussian(r2))
      policyNetwork.percept(gaussian(r3))
      policyNetwork.resetEpisode()
    }

    println("Q-Table distribution:")
    println(qTable.table mkString "\t")
    println()
    println("Q-Network distribution:")
    println(qNetwork.network.forwardProp(Array(1.0)) mkString "\t")
    println()
    println("Policy-Network policy:")
    println(policyNetwork.network.forwardProp(Array(1.0)) mkString "\t")
  }
}
