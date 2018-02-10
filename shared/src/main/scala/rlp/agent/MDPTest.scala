package rlp.agent

import rlp.ai.ActivationFunction.Linear
import rlp.ai.NeuralNetwork
import rlp.ai.optimizers.{Adam, SGDMomentum}
import rlp.environment.Environment

object MDPTest {

  val transitions: Array[Array[Int]] = Array(
    Array(1, 1),
    Array(2, 2),
    Array(3, 3),
    Array(4, 4),
    Array(5, 5),
    Array(6, 7),
    null,
    null,
  )

  val rewards: Array[Array[Double]] = Array(
    Array(0, 0),
    Array(0, 0),
    Array(0, 0),
    Array(0, 0),
    Array(0, 0),
    Array(-1, 1),
    null,
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
    qNetworkAgent.optimiser = new Adam(network, 0.003)

    qNetworkAgent.explorationEpsilon = 0.1
    qTableAgent.explorationEpsilon = 0.1

    qNetworkAgent.discountFactor = 0.9
    qTableAgent.discountFactor = 0.9

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
