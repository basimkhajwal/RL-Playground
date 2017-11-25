package rlp.agent

class QTableAgent(
  val numStates: Int, val numActions: Int,
  val table: Array[Double]
) extends SteppedAgent[Int, Int] {

  var learningRate = 0.1
  var discountFactor = 0.9
  var epsilon = 0.1

  def this(numStates: Int, numActions: Int) {
    this(numStates, numActions, new Array[Double](numStates * numActions))
  }

  override def step(prevState: Int, action: Int, reward: Double, newState: Int): Int = {
    val (greedyAction, expectedReward) = maximumAction(newState)

    this(prevState, action) += learningRate * (reward + discountFactor * expectedReward - this(prevState, action))

    greedyAction
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

object QTableAgent {

  private def stateMap[S](spaces: Array[QStateSpace[S]])(state: S): Int = {
    var stateSize = 1
    var currentIdx = 0

    for (space:QStateSpace[S] <- spaces) {
      currentIdx += stateSize * space(state)
      stateSize *= space.size
    }

    currentIdx
  }

  def build[S, A](numActions: Int, actionMap: (Int) => A, spaces: Seq[QStateSpace[S]]): (QTableAgent, Agent[S,A]) = {

    val numStates = spaces.map(_.size).product
    val agent = new QTableAgent(numStates, numActions)

    agent -> new MappedAgent[Int,Int,S,A](agent, stateMap(spaces.toArray), actionMap)
  }

}

class QStateSpace[T](val name: String, val size: Int, val map: T => Int, val defaultEnabled: Boolean) {
  def apply(s: T): Int = map(s)
}

object QStateSpace {

  def discrete[T](name: String, n: Int, map: (T) => Int, defaultEnabled: Boolean = true) = {
    new QStateSpace(name, n, map, defaultEnabled)
  }

  def boxed[T](
    name: String, low: Double, high: Double, divisions: Int = 10,
    map: T => Double, defaultEnabled: Boolean = true
  ) = {
    discrete[T](name, divisions, { s => (divisions * (map(s) - low) / (high-low)).toInt }, defaultEnabled)
  }
}
