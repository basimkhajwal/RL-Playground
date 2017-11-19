package rlp.controllers

import com.thoughtworks.binding.Binding.{BindingSeq, Constants, Vars}
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

  private val numStates = spaces.map(_.size).product

  private val spacesEnabled: Vars[(QStateSpace[O], Boolean)] = Vars(spaces.map(s => (s, s.defaultEnabled)) :_ *)

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

  private def getCheckBoxID(space: QStateSpace[O]): String = "q-enable-" + space.name

  private def checkBoxToggled(space: QStateSpace[O]): Unit = {
    val idx = spaces.indexOf(space)
    val checkBox = getElem[html.Input](getCheckBoxID(space))

    spacesEnabled.get(idx) = (space, checkBox.checked)
  }

  @dom
  override def modelOptions(enabled: Binding[Boolean]): Binding[Div] = {
    <div class="row">

      <h5 class="col offset-s1 s11 thin">Q Table Inputs</h5>
      <div class="col s12" id="q-checkbox-container">
        {
          for ((space, enabled) <- spacesEnabled) yield {
            <div class="q-table-checkbox">
              <input type="checkbox" id={getCheckBoxID(space)}
                    onchange={ _:Event => checkBoxToggled(space)}
                    checked={enabled}
                    />
              <label for={getCheckBoxID(space)}>{space.name}</label>
            </div>
          }
        }
      </div>

      <h5 class="col offset-s1 s11 thin">Q Table Parameters</h5>
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


