package rlp.controllers

import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.html.Div
import rlp.ai.agents.QTableAgent
import rlp.environment.{Agent, MappedAgent}

class QTableController[O, A](
  numActions: Int, actionMap: (Int) => A,
  spaces: QStateSpace[O]*
) extends ModelController[Agent[O, A]] {

  override val name: String = "Tabular Q Learning"

  val numStates = spaces.map(_.size).sum

  private def stateMap(state: O): Int ={
    var stateSize = 1
    var currentIdx = 0

    for (space <- spaces) {
      currentIdx += stateSize * space(state)
      stateSize *= space.size
    }

    currentIdx
  }

  @dom
  override lazy val modelOptions: Binding[Div] = {
    <div>
      Q Table Options! (to do at a later date)
    </div>
  }

  override def buildAgent(): Agent[O, A] = {
    new MappedAgent(new QTableAgent(numStates, numActions), stateMap, actionMap)
  }
}

class QStateSpace[T](val name: String, val size: Int, val map: T => Int) {
  def apply(s: T): Int = map(s)
}

object QStateSpace {

  def discrete[T](name: String, n: Int, map: (T) => Int) = new QStateSpace(name, n, map)

  def boxed[T](
    name: String, low: Double, high: Double, divisions: Int = 10,
    map: T => Double
  ) = {
    discrete[T](name, divisions, { s => (divisions * (map(s) - low) / (high-low)).toInt })
  }
}


