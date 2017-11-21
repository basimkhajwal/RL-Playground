package rlp.controllers

import com.thoughtworks.binding.Binding.{BindingSeq, Constants, Vars}
import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.{Event, document, html}
import org.scalajs.dom.html.Div
import rlp.ai.agents.{QStateSpace, QTableAgent}
import rlp.environment.{Agent, MappedAgent}
import rlp._

class QTableController[O, A](
  numActions: Int, actionMap: (Int) => A, spaces: QStateSpace[O]*
) extends ModelController[Agent[O, A]] {

  override val name: String = "Tabular Q Learning"

  private val spacesEnabled: Vars[(QStateSpace[O], Boolean)] = Vars(spaces.map(s => (s, s.defaultEnabled)) :_ *)

  val defaultLearningRate = 0.1
  val defaultForgettingFactor = 0.9

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
          for ((space, spaceEnabled) <- spacesEnabled) yield {
            <div class="q-table-checkbox">
              <input type="checkbox" id={getCheckBoxID(space)}
                    onchange={ _:Event => checkBoxToggled(space)}
                    checked={spaceEnabled} disabled={!enabled.bind}
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
    val (qAgent, agent) = QTableAgent.build(numActions, actionMap, spacesEnabled.get.filter(_._2).map(_._1))

    qAgent.learningRate = getElem[html.Input]("learningRate").value.toDouble
    qAgent.forgettingFactor = getElem[html.Input]("forgettingFactor").value.toDouble

    agent
  }
}


