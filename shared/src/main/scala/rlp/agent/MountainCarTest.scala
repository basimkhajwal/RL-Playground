package rlp.agent

import rlp.agent.QNetworkAgent.QNetworkSpace
import rlp.ai.ActivationFunction.{Linear, ReLU, Sigmoid, TanH}
import rlp.ai.NeuralNetwork
import rlp.ai.optimizers.{Adam, RMSProp, SGDMomentum}
import rlp.environment.MountainCar

import scalax.chart.api._

object MountainCarTest {

  import MountainCar._

  def runAgent(agent: MountainCarAgent): Seq[(Int, Int)] = {

    val env = new MountainCar(agent)

    for (e <- 0 until 90) yield {
      println(e)
      var length = 0
      while (!env.step() && length <= 600) {
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

    val network = new NeuralNetwork(Array(2, 5, 3), Array(ReLU, Linear))
    val qNetwork = new QNetworkAgent(network, 100)

    network.initialiseWeights()

    qNetwork.updateStepInterval = 20
    qNetwork.miniBatchSize = 50
    qNetwork.explorationEpsilon = 0.1
    qNetwork.discountFactor = 0.9
    qNetwork.optimiser = new Adam(network, 0.005, decay = 0.0005)

    val agentB = QNetworkAgent.build[State, Action](qNetwork, (x:Int) => if (x == 0) LeftAction else if (x == 1) NoAction else RightAction,
      Seq(
        QNetworkSpace.bounded[State](-0.07, 0.07, _.v),
        QNetworkSpace.bounded[State](-1.2, 0.6, _.x),
      )
    )

    XYLineChart(runAgent(agentA)).show("Q Table")
    XYLineChart(runAgent(agentB)).show("Q Network")

    /*
    val env = new MountainCar(agentB)
    val episodes = 500
    val episodeLengths = new Array[Int](episodes)
    val tdErrors = new Array[Double](episodes)
    for (e <- 0 until episodes) yield {
      var length = 0
      while (!env.step() && length <= 600) {
        length += 1
      }
      env.reset()

      episodeLengths(e) = length
      tdErrors(e) = qNetwork.totalTDError / qNetwork.errorSteps

      qNetwork.totalTDError = 0
      qNetwork.errorSteps = 0

      qNetwork.explorationEpsilon *= 0.99
    }

    val dir = "/Users/Basim/Downloads/car_test/"
    XYLineChart((0 until episodes) zip episodeLengths).saveAsPNG(dir+"ep_len.png")
    XYLineChart((0 until episodes) zip tdErrors).saveAsPNG(dir+"ep_err.png")

    println(network.weights.map(_.toString).mkString("\n"))*/
  }
}
