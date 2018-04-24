package rlp.agent.test

import rlp.agent.{Agent, MappedAgent, QNetworkAgent, QTableAgent}
import rlp.ai.ActivationFunction.Linear
import rlp.ai.NeuralNetwork
import rlp.ai.optimizers.Adam

/**
  * Integration test to see if agents can learn
  * a Markov decision process
  */
object MDPTest {

  val transitions: Array[Array[Int]] = Array(
    Array(1, 2),
    Array(1, 4),
    Array(1, 3),
    Array(1, 4),
    null,
  )

  val rewards: Array[Array[Double]] = Array(
    Array(0, 1),
    Array(-1, -2),
    Array(-2, -1),
    Array(2, 1),
    null
  )

  val numStates = transitions.length
  val numActions = transitions(0).length

  def runMDPAgent(agent: Agent[Int, Int]): Unit = {
    for (_ <- 0 until 10000) {
      var currentState = 0
      while (transitions(currentState) != null) {
        val action = agent.act(currentState)
        agent.percept(rewards(currentState)(action))
        currentState = transitions(currentState)(action)
      }
      agent.resetEpisode()
    }
  }

  def oneHot(size: Int)(idx: Int): Array[Double] = {
    val arr = new Array[Double](size)
    arr(idx) = 1
    arr
  }

  def main(args: Array[String]): Unit = {

    val network = new NeuralNetwork(Array(numStates, numActions), Array(Linear))
    val qNetworkAgent = new QNetworkAgent(network, 1)
    val qTableAgent = new QTableAgent(numStates, numActions)

    qNetworkAgent.updateStepInterval = 1
    qNetworkAgent.optimiser = new Adam(network, 0.0001)

    qNetworkAgent.explorationEpsilon = 0.1
    qTableAgent.explorationEpsilon = 0.1

    qNetworkAgent.discountFactor = 1
    qTableAgent.discountFactor = 1

    runMDPAgent(new MappedAgent[Array[Double],Int,Int,Int](qNetworkAgent, oneHot(numStates), identity))
    runMDPAgent(qTableAgent)

    for (state <- 0 until numStates) {
      if (transitions(state) != null) {
        for (action <- 0 until numActions) {
          val qNetOut = network.forwardProp(oneHot(numStates)(state))(action)
          println(s"$state \t $action \t ${qTableAgent(state, action)} \t $qNetOut")
        }
      }
    }
  }
}
