package rlp.agent

import rlp.agent.QNetworkAgent.QNetworkSpace
import rlp.ai.ActivationFunction.{Linear, Sigmoid, TanH}
import rlp.ai.NeuralNetwork
import rlp.ai.optimizers.{Adam, SGDMomentum}
import rlp.environment.MountainCar

import scalax.chart.api._

object MountainCarTest {

  import MountainCar._

  def runAgent(agent: MountainCarAgent): Seq[(Int, Int)] = {

    val env = new MountainCar(agent)

    for (e <- 0 until 5000) yield {
      var length = 0
      while (!env.step() && length <= 3000) {
        length += 1
      }
      env.reset()

      (e, length)
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

    val network = new NeuralNetwork(Array(2, 10, 3), Array(TanH, Linear))
    val qNetwork = new QNetworkAgent(network, 100)

    network.initialiseWeights()

    qNetwork.updateStepInterval = 10
    qNetwork.explorationEpsilon = 0.1
    qNetwork.discountFactor = 0.9
    qNetwork.optimiser = new SGDMomentum(network, 0.0001)

    val agentB = QNetworkAgent.build[State, Action](qNetwork, (x:Int) => if (x == 0) LeftAction else if (x == 1) NoAction else RightAction,
      Seq(
        QNetworkSpace.bounded[State](-0.07, 0.07, _.v),
        QNetworkSpace.bounded[State](-1.2, 0.6, _.x),
      )
    )

    val chart1 = XYLineChart(runAgent(agentA))
    chart1.show("Q Table")

    val chart2 = XYLineChart(runAgent(agentB))
    chart2.show("Q Network")
  }
}
