package rlp.ai.agents

import rlp.environment.SARSAAgent

class QTableAgent(
  val dimensions: Array[Int],
  val numActions: Int,
  val table: Array[Double]
) extends LearningAgent with SARSAAgent[Array[Double], Int] {

  private val stateDimensions = dimensions.product
  private var spaces: Array[Space] = _

  var learningRate = 0.1
  var forgettingFactor = 0.9

  def this(dimensions: Array[Int], numActions: Int) {
    this(dimensions, numActions, new Array[Double](dimensions.product * numActions))
  }

  override def initialise(spaces: Space*): Unit = {
    this.spaces = spaces.toArray
  }

  override def sarsa(prevState: Array[Double], action: Int, reward: Double, newState: Array[Double]): Int = {
    val (bestAction, expectedReward) = maximumAction(index(newState))

    if (prevState != null) {
      val pidx = index(prevState, action)

      table(pidx) += learningRate * (reward + forgettingFactor * expectedReward - table(pidx))
    }

    bestAction
  }

  private def maximumAction(stateIdx: Int): (Int, Double) = {
    var maxAction = 0
    var maxActionValue = Double.NegativeInfinity
    for (a <- 0 until numActions) {
      val actionValue = table(index(stateIdx, a))
      if (actionValue > maxActionValue) {
        maxActionValue = actionValue
        maxAction = a
      }
    }
    (maxAction, maxActionValue)
  }

  private def index(state: Array[Double]): Int = {
    var idx = 0
    var dimSize = 1
    for (i <- spaces.indices) {
      spaces(i) match {
        case BoxedSpace(l, h, d) => {
          idx += dimSize * (d * (state(i) - l) / (h - l)).toInt
          dimSize *= d
        }
        case DiscreteSpace(n) => {
          idx += dimSize * state(i).toInt
          dimSize *= n
        }
      }
    }
    idx
  }

  private def index(state: Array[Double], action: Int): Int = {
    index(state) + stateDimensions * action
  }

  private def index(stateIdx: Int, action: Int): Int = {
    stateIdx + stateDimensions * action
  }
}
