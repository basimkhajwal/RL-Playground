package rlp.ai.agents

class QTableAgent(val dimensions: Array[Int], val numActions: Int, val table: Array[Double]) extends LearningAgent {

  private val stateDimensions = dimensions.product
  private var spaces: Array[Space] = _

  def this(dimensions: Array[Int], numActions: Int) {
    this(dimensions, numActions, new Array[Double](dimensions.product * numActions))
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

  override def initialise(spaces: Space*): Unit = {
    this.spaces = spaces.toArray
  }

  override def act(state: Array[Double]): Int = {

    for (a <- 0 until numActions) {

    }
  }
}
