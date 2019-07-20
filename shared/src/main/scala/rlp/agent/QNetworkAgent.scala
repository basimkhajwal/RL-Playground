package rlp.agent

import rlp.ai.NeuralNetwork
import rlp.ai.optimizers.{Adam, NetworkOptimizer, SGDMomentum}
import rlp.math.Matrix
import ujson.Js
import upickle.default._

import scala.reflect.ClassTag
import scala.util.Random

class QNetworkAgent(
  val network: NeuralNetwork,
  val replayBufferSize: Int = 10000,
  val seed: Long = System.currentTimeMillis()
) extends SteppedAgent[Array[Double], Int]{

  type Replay = (Array[Double], Int, Double, Array[Double])

  private val replayBuffer = new Array[Replay](replayBufferSize)
  private var stepCount = 0

  var optimiser: NetworkOptimizer = new SGDMomentum(network, 0.0001)
  var discountFactor = 0.99
  var explorationEpsilon = 0.1
  var miniBatchSize = math.min(10, replayBufferSize)
  var updateStepInterval = 50

  private val rand = new Random(seed)

  /** Run the Fisher-Yates to generate random indices
    * for selecting k elements from n elements
    */
  private def sampleIndices(n: Int, k: Int): Array[Int] = {
    val arr = new Array[Int](k)

    for (i <- 0 until k) arr(i) = i
    for (i <- k until n) {
      val j = rand.nextInt(i + 1)
      if (j < k) arr(j) = i
    }

    arr
  }

  /** Select k uniformly random elements from an array of length n */
  private def sampleItems[T : ClassTag](
    xs: Array[T],
    n: Int, k: Int): Array[T] = {
    sampleIndices(n, k).map(xs)
  }

  override def step(
    prevState: Array[Double], action: Int, reward: Double,
    newState: Array[Double], first: Boolean, last: Boolean): Int = {

    val numActions = network.layerSizes(network.numLayers - 1)

    if (!first && isTrainEnabled()) {

      // Store the replay in the circular buffer
      replayBuffer(stepCount % replayBufferSize) =
        (prevState, action, reward, if (last) null else newState)
      stepCount += 1

      // If the buffer is full and the step interval is reached
      if (stepCount >= miniBatchSize && stepCount % updateStepInterval == 0) {

        // Randomly sample replays
        val sampleSize = Math.min(stepCount, replayBufferSize)
        val sample = sampleItems(replayBuffer, sampleSize, miniBatchSize)

        // Extract inputs and current returns
        val inputs = Matrix.rows(sample.map(_._1))
        val returns = network.forwardProp(inputs)

        // Work out expected rewards via Q-learning algorithm
        for (i <- 0 until miniBatchSize) {
          val (_, a, r, n) = sample(i)
          returns(i, a) = r

          if (n != null) {
            returns(i, a) += discountFactor * maxAction(n)._2
          }
        }

        // Step the optimiser to reduce error
        optimiser.step(inputs, returns)
      }
    }

    // Check for exploration rate
    if (rand.nextDouble() < explorationEpsilon)
      (rand.nextDouble() * numActions).toInt // random action
    else
      maxAction(newState)._1 // greedy action
  }


  // Extract the maximum action (with its value) from a state
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

  override def load(data: ujson.Value): Unit = {
    val keyMap = data.obj

    network.load(keyMap("network"))
    optimiser = NetworkOptimizer.create(network, keyMap("optimiser"))

    discountFactor = keyMap("discountFactor").num
    explorationEpsilon = keyMap("explorationEpsilon").num
    miniBatchSize = keyMap("miniBatchSize").num.toInt
    updateStepInterval = keyMap("updateStepInterval").num.toInt

    stepCount = keyMap("stepCount").num.toInt
    val replayData = read[Seq[Replay]](keyMap("replayBuffer")).toArray
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

  def build[S,A](
    qAgent: QNetworkAgent, actionMap: Int => A, spaces: Seq[QNetworkSpace[S]]
  ): Agent[S,A] = {
    new MappedAgent(qAgent, stateMap(spaces.toArray), actionMap)
  }
}
