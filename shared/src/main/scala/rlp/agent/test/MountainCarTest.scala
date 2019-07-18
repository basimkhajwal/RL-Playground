package rlp.agent.test

import rlp.agent.QNetworkAgent.QNetworkSpace
import rlp.agent.{QNetworkAgent, QTableAgent}
import rlp.agent.QTableAgent.QStateSpace
import rlp.ai.ActivationFunction.{Linear, ReLU}
import rlp.ai.NeuralNetwork
import rlp.ai.optimizers.Adam
import rlp.environment.MountainCar

import scalax.chart.api._

/**
  * Integration test involving the mountain car agent
  */
object MountainCarTest {

  import MountainCar._

  def runAgent(agent: MountainCarAgent): Seq[(Int, Int)] = {

    val env = new MountainCar(agent)

    for (e <- 0 until 10000) yield {
      //println(e)
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
        QStateSpace.boxed[State](-0.07, 0.07, 100, _.v),
        QStateSpace.boxed[State](-1.2, 0.6, 100, _.x),
      )
    )

    qTable.explorationEpsilon = 1
    qTable.learningRate = 0.5

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

    val numSteps = 20000

    val targetEpsilon = 0.01
    val targetLearningRate = 0.1

    val epsilonDecay = Math.pow(targetEpsilon / qTable.explorationEpsilon, 1.0 / numSteps)
    val learningDecay = Math.pow(targetLearningRate / qTable.learningRate, 1.0 / numSteps)

    val env = new MountainCar(agentA)
    val data =
      for (e <- 0 until numSteps) yield {

        if (e % 100 == 0) println(e)

        var length = 0
        while (!env.step() && length <= 1000) {
          length += 1
        }
        env.reset()

        qTable.explorationEpsilon *= epsilonDecay
        qTable.learningRate *= learningDecay

        (e, length)
      }

    XYLineChart(data).show("Q Table")
   // XYLineChart(runAgent(agentB)).show("Q Network")

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
