package rlp.agent.test

import rlp.agent.QTableAgent.QStateSpace
import rlp.agent.{MappedAgent, QNetworkAgent, QTableAgent}
import rlp.ai.ActivationFunction.Linear
import rlp.ai.NeuralNetwork
import rlp.ai.optimizers.Adam
import rlp.environment.{NaivePongAgent, Pong}
import rlp.util.Point2D

import scala.collection.mutable.ArrayBuffer
import scalax.chart.module.XYChartFactories.XYLineChart

/**
  * Pong integration test
  */
object PongTest {

  import rlp.environment.Pong._

  def main(args: Array[String]): Unit = {

    // val qNetworkAgent = new PolicyNetworkAgent(new NeuralNetwork(Array(3, 2), Array(Linear), true))
    val agent = new QNetworkAgent(new NeuralNetwork(Array(3, 2), Array(Linear)), 1)
    agent.network.initialiseWeights()
    agent.optimiser = new Adam(agent.network, 0.001)

    //val mapped = new MappedAgent[Array[Double],Int,AgentState,Action](
    //  agent,
    //  state => Array(2 * state.ballPos.x / SCREEN_WIDTH - 1, 2 * state.ballPos.y / SCREEN_HEIGHT - 1, 2 * state.currentPaddle / SCREEN_HEIGHT - 1),
    //  action => if (action == 0) UpAction else DownAction
    //)

    val (qTable, mapped) = QTableAgent.build[Pong.AgentState, Pong.Action](
      3,
      (action:Int) => if (action == 0) UpAction else if (action == 1) DownAction else NoAction,
      List(
        QStateSpace.boxed[Pong.AgentState](0, SCREEN_WIDTH, 20, _.ballPos.x),
        QStateSpace.boxed[Pong.AgentState](0, SCREEN_HEIGHT, 20, _.ballPos.y),
        QStateSpace.boxed[Pong.AgentState](0, SCREEN_HEIGHT, 20, _.currentPaddle)
      )
    )

    val pong = new Pong(mapped, new NaivePongAgent)

    var totalSteps = 0
    val logSize = 10000
    val numSteps = 10000000

    qTable.explorationEpsilon = 0.5
    qTable.learningRate = 0.2
    val epsilonDecay = math.pow(0.01 / qTable.explorationEpsilon, 1.0 / numSteps)
    val learningDecay = math.pow(0.07 / qTable.learningRate, 1.0 / numSteps)

    val resultBuffer = ArrayBuffer[(Int,Double)]()

    for (i <- 0 until numSteps) {
      var steps = 0
      while (steps < 1500 && !pong.step()) steps += 1
      pong.reset()

      qTable.explorationEpsilon *= epsilonDecay
      qTable.learningRate *= learningDecay

      totalSteps += steps

      if (i % logSize == logSize-1) {
        println(s"${i+1} - avg. episode size: ${totalSteps/(logSize * 1.0)}")
        resultBuffer += ((i, totalSteps*1.0/logSize))
        totalSteps = 0
      }
    }

    XYLineChart(resultBuffer).show("Episode size over time")
  }
}
