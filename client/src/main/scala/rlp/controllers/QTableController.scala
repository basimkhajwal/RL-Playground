package rlp.controllers

import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.{Event, document, html}
import org.scalajs.dom.html.Div
import rlp.ai.agents.QTableAgent
import rlp.environment.{Agent, MappedAgent}
import rlp._

class QTableController[O, A](
  numActions: Int, actionMap: (Int) => A,
  spaces: QStateSpace[O]*
) extends ModelController[Agent[O, A]] {

  override val name: String = "Tabular Q Learning"

  val numStates = spaces.map(_.size).product

  val defaultLearningRate = 0.1
  val defaultForgettingFactor = 0.9

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
  override def modelOptions(enabled: Binding[Boolean]): Binding[Div] = {
    <div class="row">

      <div class="input-field col s3 offset-s2">
        <input id="learningRate" class="validate" type="number" min="0" step="any" value={defaultLearningRate.toString}
          oninput={_:Event => validate()} disabled={!enabled.bind}/>
        <label for="learningRate" data:data-error="Learning rate must be positive">Learning Rate</label>
      </div>

      <div class="input-field col s3 offset-s2">
        <input id="forgettingFactor" class="validate" type="number" min="0" max="1" step="any" value={defaultForgettingFactor.toString}
          oninput={_:Event => validate()} disabled={!enabled.bind}/>
        <label for="forgettingFactor" data:data-error="Forgetting factor must be between 0 and 1">Forgetting Factor</label>
      </div>

    </div>
  }

  private def getElem[T](id: String): T = document.getElementById(id).asInstanceOf[T]

  override def validate(): Boolean = {
    val learningRate = getElem[html.Input]("learningRate")
    val forgettingFactor = getElem[html.Input]("forgettingFactor")

    learningRate.validity.valid && forgettingFactor.validity.valid
  }

  override def buildAgent(): Agent[O, A] = {
    val qTable = new QTableAgent(numStates, numActions)

    qTable.learningRate = getElem[html.Input]("learningRate").value.toDouble
    qTable.forgettingFactor = getElem[html.Input]("forgettingFactor").value.toDouble

    new MappedAgent(qTable, stateMap, actionMap)
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


