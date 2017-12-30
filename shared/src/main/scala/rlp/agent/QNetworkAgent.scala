package rlp.agent

import rlp.ai.NeuralNetwork
import rlp.ai.optimizers.{Adam, NetworkOptimizer, SGDMomentum}
import rlp.math.Matrix
import upickle.Js
import upickle.default._

import scala.reflect.ClassTag
import scala.util.Random

class QNetworkAgent(
  val network: NeuralNetwork,
  val replayBufferSize: Int = 10000,
) extends SteppedAgent[Array[Double], Int]{

  type Replay = (Array[Double], Int, Double, Array[Double])

  private val replayBuffer = new Array[Replay](replayBufferSize)
  private var stepCount = 0

  var optimiser: NetworkOptimizer = new SGDMomentum(network, 0.0001)
  var discountFactor = 0.99
  var explorationEpsilon = 0.1
  var miniBatchSize = 10
  var updateStepInterval = 50

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

  private def sampleItems[T : ClassTag](xs: Array[T], n: Int, k: Int): Array[T] = sampleIndices(n, k).map(xs)

  override def step(prevState: Array[Double], action: Int, reward: Double, newState: Array[Double], first: Boolean, last: Boolean): Int = {

    if (!first) {

      replayBuffer(stepCount % replayBufferSize) = (prevState, action, reward, if (last) null else newState)
      stepCount += 1

      if (stepCount >= miniBatchSize && stepCount % updateStepInterval == 0) {
        val sample = sampleItems(replayBuffer, Math.min(stepCount, replayBufferSize), miniBatchSize)

        val inputs = Matrix.rows(sample.map(_._1))

        val returns = network.forwardProp(inputs)

        for (i <- 0 until miniBatchSize) {
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

    if (math.random() < explorationEpsilon) (math.random() * numActions).toInt
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

    discountFactor = keyMap("discountFactor").num
    explorationEpsilon = keyMap("explorationEpsilon").num
    miniBatchSize = keyMap("miniBatchSize").num.toInt
    updateStepInterval = keyMap("updateStepInterval").num.toInt

    stepCount = keyMap("stepCount").num.toInt
    val replayData = readJs[Seq[Replay]](keyMap("replayBuffer")).toArray
    for (i <- replayBuffer.indices) replayBuffer(i) = replayData(i)
  }

  override def store(): Js.Value = {
    Js.Obj(
      "network" -> network.store(),
      "optimiser" -> NetworkOptimizer.store(optimiser),

      "discountFactor" -> Js.Num(discountFactor),
      "explorationEpsilon" -> Js.Num(explorationEpsilon),
      "miniBatchSize" -> Js.Num(miniBatchSize),
      "updateStepInterval" -> Js.Num(updateStepInterval),

      "stepCount" -> Js.Num(stepCount),
      "replayBuffer" -> writeJs(replayBuffer.toSeq)
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
