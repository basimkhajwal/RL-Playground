package rlp.ai.agents

import rlp.environment.SARSAAgent

class QTableAgent(
  val numStates: Int, val numActions: Int,
  val table: Array[Double]
) extends SARSAAgent[Int, Int] {

  var learningRate = 0.1
  var forgettingFactor = 0.9
  var epsilon = 0.1

  def this(numStates: Int, numActions: Int) {
    this(numStates, numActions, new Array[Double](numStates * numActions))
  }

  override def sarsa(prevState: Int, action: Int, reward: Double, newState: Int): Int = {
    val (greedyAction, expectedReward) = maximumAction(newState)

    this(prevState, action) += learningRate * (reward + forgettingFactor * expectedReward - this(prevState, action))

    greedyAction
  }

  private def actionProbabilities(state: Int): Array[Double] = {
    val values = new Array[Double](numActions)
    var total = 0.0
    var maxValue = Double.NegativeInfinity

    for (a <- 0 until numActions) {
      values(a) = this(state,a)
      if (values(a) > maxValue) maxValue = values(a)
    }

    for (a <- 0 until numActions) {
      values(a) = math.exp(values(a) - maxValue)
      total += values(a)
    }

    for (a <- 0 until numActions) {
      values(a) /= total
    }

    values
  }


  private def maximumAction(state: Int): (Int, Double) = {
    var maxAction = 0
    var maxActionValue = Double.NegativeInfinity
    for (a <- 0 until numActions) {
      val actionValue = this(state, a)
      if (actionValue > maxActionValue) {
        maxActionValue = actionValue
        maxAction = a
      }
    }
    (maxAction, maxActionValue)
  }

  @inline
  def apply(state: Int, action: Int): Double = table(state + action * numStates)

  @inline
  def update(state: Int, action: Int, value: Double): Unit = {
    table(state + action * numStates) = value
  }
}
