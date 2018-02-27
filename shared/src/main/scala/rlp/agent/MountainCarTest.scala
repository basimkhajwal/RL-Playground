package rlp.agent

import rlp.agent.QNetworkAgent.QNetworkSpace
import rlp.ai.ActivationFunction.{Linear, Sigmoid}
import rlp.ai.NeuralNetwork
import rlp.ai.optimizers.{Adam, SGDMomentum}
import rlp.environment.MountainCar

object MountainCarTest {

  import MountainCar._

  def runAgent(agent: MountainCarAgent): Unit = {

    val env = new MountainCar(agent)

    var runningLength = 0

    for (e <- 0 until 10000) {

      var length = 0
      while (!env.step() && length <= 5000) {
        length += 1
      }

      runningLength += length
      env.reset()

      if ((e+1) % 100 == 0) {
        println((e+1) + ": " + (runningLength / 100.0))
        runningLength = 0
      }
    }

  }

  def main(args: Array[String]): Unit = {

    val (qTable, agentA) = QTableAgent.build[State, Action](3, (x:Int) => if (x == 0) LeftAction else if (x == 1) NoAction else RightAction,
      QStateSpace.combination(
        QStateSpace.boxed[State](-0.07, 0.07, 50, _.v),
        QStateSpace.boxed[State](-1.2, 0.6, 50, _.x),
      )
    )

    qTable.explorationEpsilon = 0.05

    val network = new NeuralNetwork(Array(2, 3), Array(Linear))
    val qNetwork = new QNetworkAgent(network, 100)

    network.initialiseWeights()

    qNetwork.updateStepInterval = 10
    qNetwork.explorationEpsilon = 0.1
    qNetwork.discountFactor = 0.9
    qNetwork.optimiser = new Adam(network, 0.0001)

    val agentB = QNetworkAgent.build[State, Action](qNetwork, (x:Int) => if (x == 0) LeftAction else if (x == 1) NoAction else RightAction,
      Seq(
        QNetworkSpace.bounded[State](-0.07, 0.07, _.v),
        QNetworkSpace.bounded[State](-1.2, 0.6, _.x),
      )
    )

    println("Q Table...")
    runAgent(agentA)
    println("\nQ Network...")
    runAgent(agentB)
  }
}
