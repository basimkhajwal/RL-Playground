package rlp.agent
import rlp.storage.Storable
import ujson.Js

class QTableAgent(
  val numStates: Int, val numActions: Int,
  val table: Array[Double]
) extends SteppedAgent[Int, Int] {

  var learningRate = 0.1
  var discountFactor = 0.9
  var explorationEpsilon = 0.1

  def this(numStates: Int, numActions: Int) {
    this(numStates, numActions, new Array[Double](numStates * numActions))
  }

  override def step(
    prevState: Int, action: Int, reward: Double,
    newState: Int, first: Boolean, last: Boolean
  ): Int = {

    // Extract the best action and reward from this state
    val (greedyAction, maxReward) = maximumAction(newState)

    if (!first && isTrainEnabled()) {
      
      // Apply Q-learning step algorithm
      val expectedReward = reward + discountFactor * (if (last) 0 else maxReward)
      val tdError = expectedReward - this(prevState, action)

      this(prevState, action) += learningRate * tdError
    }

    // Pick a random action based on exploration rate,
    // or pick the best action
    if (math.random() < explorationEpsilon)
      (math.random() * numActions).toInt
    else greedyAction
  }

  /**
    * Extract the maximum action and reward from the state
    */
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

  override def reset(): Unit = {
    for (i <- table.indices) table(i) = 0
  }

  override def duplicate(): QTableAgent = {
    new QTableAgent(numStates, numActions, table.clone())
  }

  @inline
  def apply(state: Int, action: Int): Double = {
    table(state + action * numStates)
  }

  @inline
  def update(state: Int, action: Int, value: Double): Unit = {
    table(state + action * numStates) = value
  }

  override def load(data: Js.Value): Unit = {

    val keyMap = data.obj

    learningRate = keyMap("learningRate").num
    discountFactor = keyMap("discountFactor").num
    explorationEpsilon = keyMap("explorationEpsilon").num

    val xs = Storable.binaryRead(keyMap("table").str.toString)
    for (i <- table.indices) table(i) = xs(i)
  }

  override def store(): Js.Value = {
    Js.Obj(
      "learningRate" -> Js.Num(learningRate),
      "discountFactor" -> Js.Num(discountFactor),
      "explorationEpsilon" -> Js.Num(explorationEpsilon),
      "table" -> Storable.binaryStore(table)
    )
  }
}

object QTableAgent {

  class QStateSpace[T](val size: Int, val map: T => Int) {
    def apply(s: T): Int = map(s)
  }

  object QStateSpace {

    private def stateMap[S](spaces: Array[QStateSpace[S]])(state: S): Int = {
      var stateSize = 1
      var currentIdx = 0

      for (space:QStateSpace[S] <- spaces) {
        currentIdx += stateSize * space(state)
        stateSize *= space.size
      }

      currentIdx
    }

    def discrete[T](n: Int, map: (T) => Int) = new QStateSpace(n, map)

    def boxed[T](
      low: Double, high: Double, divisions: Int = 10, map: T => Double
    ): QStateSpace[T] = {
      discrete[T](divisions, { s => (divisions * (map(s) - low) / (high-low)).toInt })
    }

    def combination[T](spaces: QStateSpace[T]*): QStateSpace[T] = {
      combination(spaces.toArray)
    }

    def combination[T](spaces: Array[QStateSpace[T]]): QStateSpace[T] = {
      new QStateSpace[T](spaces.map(_.size).product, stateMap(spaces))
    }
  }

  def build[S,A](
    numActions: Int, actionMap: (Int) => A, space: QStateSpace[S]
  ): (QTableAgent, Agent[S,A]) = {

    val agent = new QTableAgent(space.size, numActions)

    agent -> new MappedAgent[Int,Int,S,A](agent, space.map, actionMap)
  }

  def build[S, A](
    numActions: Int, actionMap: (Int) => A, spaces: Seq[QStateSpace[S]]
  ): (QTableAgent, Agent[S,A]) = {
    build(numActions, actionMap, QStateSpace.combination(spaces.toArray))
  }
}

