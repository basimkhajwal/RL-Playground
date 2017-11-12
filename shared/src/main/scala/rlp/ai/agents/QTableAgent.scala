package rlp.ai.agents

import rlp.environment.SARSAAgent

class QTableAgent(
  val numStates: Int, val numActions: Int,
  val table: Array[Double]
) extends SARSAAgent[Int, Int] {

  var learningRate = 0.1
  var forgettingFactor = 0.9

  def this(numStates: Int, numActions: Int) {
    this(numStates, numActions, new Array[Double](numStates * numActions))
  }

  override def sarsa(prevState: Int, action: Int, reward: Double, newState: Int): Int = {
    val (bestAction, expectedReward) = maximumAction(newState)

    this(prevState, action) += learningRate * (reward + forgettingFactor * expectedReward - this(prevState, action))

    bestAction
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
