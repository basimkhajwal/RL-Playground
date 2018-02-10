package rlp.agent

import rlp.ai.ActivationFunction.{Linear, ReLU}
import rlp.ai.NeuralNetwork
import rlp.ai.optimizers.{Adam, SGDMomentum}
import rlp.environment.{NaivePongAgent, Pong}
import rlp.util.Point2D

object PongTest {

  import rlp.environment.Pong._

  def main(args: Array[String]): Unit = {

    // val qNetworkAgent = new PolicyNetworkAgent(new NeuralNetwork(Array(3, 2), Array(Linear), true))
    val agent = new QNetworkAgent(new NeuralNetwork(Array(3, 2), Array(Linear)), 1)
    agent.network.initialiseWeights()
    agent.optimiser = new Adam(agent.network, 0.001)

    val mapped = new MappedAgent[Array[Double],Int,AgentState,Action](
      agent,
      state => Array(2 * state.ballPos.x / SCREEN_WIDTH - 1, 2 * state.ballPos.y / SCREEN_HEIGHT - 1, 2 * state.currentPaddle / SCREEN_HEIGHT - 1),
      action => if (action == 0) UpAction else DownAction
    )

    val pong = new Pong(mapped, mapped.clone())
    val naive = new NaivePongAgent

    var totalSteps = 0
    val logSize = 500

    for (i <- 0 until 50000) {
      var steps = 0
      while (steps < 1500 && !pong.step()) steps += 1
      pong.reset()

      totalSteps += steps

      if (i % logSize == logSize-1) {

        mapped.setTrainEnabled(false)
        agent.explorationEpsilon = 0

        var correct = 0
        for (_ <- 0 until 1000) {
          val test = AgentState(
            Point2D(math.random() * SCREEN_WIDTH, math.random() * SCREEN_HEIGHT),
            Point2D.fromPolar(math.random()*2*math.Pi, 1), math.random(), math.random())

          if (naive.act(test) == mapped.act(test)) correct += 1
        }

        agent.explorationEpsilon = 0.1
        mapped.setTrainEnabled(true)

        println()
        println((i+1) + ":")
        println(agent.network.weights(0).toString.replace("\n", " "))
        println(s"Optimal actions: $correct / 1000")
        println(s"Avg. episode size: ${totalSteps/(logSize * 1.0)}")

        totalSteps = 0
      }
    }
  }
}
