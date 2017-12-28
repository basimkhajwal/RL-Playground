package rlp.agent

import rlp.ai.NeuralNetwork
import rlp.ai.optimizers.{Adam, NetworkOptimizer, SGDMomentum}
import rlp.math.Matrix
import upickle.Js

import scala.reflect.ClassTag
import scala.util.Random

class QNetworkAgent(
  val network: NeuralNetwork
) extends SteppedAgent[Array[Double], Int]{

  type Replay = (Array[Double], Int, Double, Array[Double])

  val replayBuffer = new Array[Replay](100)
  var replayBufferIdx = 0
  var isFull = false

  var optimiser: NetworkOptimizer = new SGDMomentum(network, 0.0001)
  val discountFactor = 0.99

  private def sampleIndices(n: Int, k: Int): Array[Int] = {
    val arr = new Array[Int](k)
    val rand = new Random()

    for (i <- 0 until k) arr(i) = i
    for (i <- k until n) {
      val j = rand.nextInt(i + 1)
      if (j < k) arr(j) = i
    }

    arr
  }

  private def sampleItems[T : ClassTag](xs: Array[T], n: Int, k: Int): Array[T] = {
    sampleIndices(n, k).map(xs).toArray
  }

  override def step(prevState: Array[Double], action: Int, reward: Double, newState: Array[Double], first: Boolean, last: Boolean): Int = {

    if (!first) {

      /* Store replay */
      replayBuffer(replayBufferIdx) = (prevState, action, reward, if (last) null else newState)
      replayBufferIdx += 1
      if (replayBufferIdx >= replayBuffer.length) {
        replayBufferIdx = 0
        isFull = true
      }

      val minibatchSize = 10
      val numItems = if (isFull) replayBuffer.length else replayBufferIdx

      if (numItems >= minibatchSize) {
        val sample = sampleItems(replayBuffer, numItems, minibatchSize)

        val inputs = Matrix.rows(sample.map(_._1))

        val returns = network.forwardProp(inputs)

        for (i <- 0 until minibatchSize) {
          val (_, a, r, n) = sample(i)

          if (n == null) {
            returns(i, a) = r
          } else {
            returns(i, a) = r + discountFactor * maxAction(n)._2
          }
        }

        optimiser.step(inputs, returns)
      }
    }

    val numActions = network.layerSizes(network.numLayers - 1)

    if (math.random() < 0.005) (math.random() * numActions).toInt
    else maxAction(newState)._1
  }

  private def maxAction(state: Array[Double]): (Int, Double) = {
    val result = network.forwardProp(state)

    var maxIdx = 0
    var maxAns = Double.NegativeInfinity
    for (i <- result.indices) {
      if (result(i) > maxAns) {
        maxAns = result(i)
        maxIdx = i
      }
    }

    (maxIdx, maxAns)
  }

  override def reset(): Unit = {
    network.initialiseWeights()
  }

  override def load(data: Js.Value): Unit = {
    val keyMap = data.obj
    network.load(keyMap("network"))
    optimiser = NetworkOptimizer.create(network, keyMap("optimiser"))
  }

  override def store(): Js.Value = {
    Js.Obj(
      "network" -> network.store(),
      "optimiser" -> NetworkOptimizer.store(optimiser)
    )
  }
}

object QNetworkAgent {

  case class QNetworkSpace[T](size: Int, map: T => Array[Double]) {
    def apply(state: T) = map(state)
  }

  object QNetworkSpace {
    def bounded[T](min: Double, max: Double, map: T => Double): QNetworkSpace[T] = {
      QNetworkSpace(1, s => Array((map(s) - min) / (max - min)))
    }
  }

  def stateMap[S](spaces: Array[QNetworkSpace[S]])(state: S): Array[Double] = {
    spaces.flatMap(_(state))
  }

  def build[S,A](qAgent: QNetworkAgent, actionMap: Int => A, spaces: Seq[QNetworkSpace[S]]): Agent[S,A] = {
    new MappedAgent(qAgent, stateMap(spaces.toArray), actionMap)
  }
}
