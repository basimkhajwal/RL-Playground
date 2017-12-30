package rlp.agent
import upickle.Js

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

  override def step(prevState: Int, action: Int, reward: Double, newState: Int, first: Boolean, last: Boolean): Int = {
    val (greedyAction, maxReward) = maximumAction(newState)

    val newAction = if (math.random() < explorationEpsilon) (math.random() * numActions).toInt else greedyAction

    if (!first) {

      val expectedReward = reward + discountFactor * (if (last) 0 else maxReward)

      this(prevState, action) += learningRate * (expectedReward - this(prevState, action))
    }

    newAction
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

  override def reset(): Unit = {
    for (i <- table.indices) table(i) = 0
  }

  override def duplicate(): QTableAgent = new QTableAgent(numStates, numActions, table.clone())

  @inline
  def apply(state: Int, action: Int): Double = table(state + action * numStates)

  @inline
  def update(state: Int, action: Int, value: Double): Unit = {
    table(state + action * numStates) = value
  }

  override def load(data: Js.Value): Unit = {

    val keyMap = data.obj

    learningRate = keyMap("learningRate").num
    discountFactor = keyMap("discountFactor").num
    explorationEpsilon = keyMap("explorationEpsilon").num

    val xs = keyMap("table").arr.map(_.num)
    for (i <- table.indices) {
      table(i) = xs(i)
    }
  }

  override def store(): Js.Value = {
    Js.Obj(
      "learningRate" -> Js.Num(learningRate),
      "discountFactor" -> Js.Num(discountFactor),
      "explorationEpsilon" -> Js.Num(explorationEpsilon),
      "table" -> Js.Arr(table.map(Js.Num) :_*)
    )
  }
}

object QTableAgent {

  def build[S,A](numActions: Int, actionMap: (Int) => A, space: QStateSpace[S]): (QTableAgent, Agent[S,A]) = {
    val agent = new QTableAgent(space.size, numActions)

    agent -> new MappedAgent[Int,Int,S,A](agent, space.map, actionMap)
  }

  def build[S, A](numActions: Int, actionMap: (Int) => A, spaces: Seq[QStateSpace[S]]): (QTableAgent, Agent[S,A]) = {
    build(numActions, actionMap, QStateSpace.combination(spaces.toArray))
  }
}

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

  def boxed[T](low: Double, high: Double, divisions: Int = 10, map: T => Double) = {
    discrete[T](divisions, { s => (divisions * (map(s) - low) / (high-low)).toInt })
  }

  def combination[T](spaces: QStateSpace[T]*): QStateSpace[T] = combination(spaces.toArray)

  def combination[T](spaces: Array[QStateSpace[T]]): QStateSpace[T] = {
    new QStateSpace[T](spaces.map(_.size).product, stateMap(spaces))
  }
}
