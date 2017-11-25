package rlp.controllers

import com.thoughtworks.binding.Binding.{BindingSeq, Constants, Var, Vars}
import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.{Event, document, html}
import org.scalajs.dom.html.Div
import org.scalajs.dom.raw.HTMLElement
import rlp._
import rlp.agent.{Agent, QStateSpace, QTableAgent}

class QTableController[O, A](
  numActions: Int, actionMap: (Int) => A, spaces: Array[QStateSpace[O]]
) extends ModelController[Agent[O, A]] {

  override val name: String = "Tabular Q Learning"

  private val spacesEnabled: Vars[(QStateSpace[O], Boolean)] = Vars(spaces.map(s => (s, s.defaultEnabled)) :_ *)

  private val learningRate = Var(0.1)
  private val discountFactor = Var(0.9)

  private var qTable: QTableAgent = _

  @dom
  override lazy val modelBuilder: Binding[HTMLElement] = {
    <div class="row">

      <h5 class="col offset-s1 s11 light">Q Table Inputs</h5>
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
    </div>
  }


  override def buildAgent(): Agent[O, A] = {
    val (qAgent, agent) = QTableAgent.build(numActions, actionMap, spacesEnabled.get.filter(_._2).map(_._1))

    learningRate := qAgent.learningRate
    discountFactor := qAgent.discountFactor
    qTable = qAgent

    agent
  }

  @dom
  override lazy val modelViewer: Binding[HTMLElement] = {
    <div>
      <h5 class="col offset-s1 s11 light">Q Table Parameters</h5>

      <div class="input-field col s3 offset-s2">
        <input id="learning-rate" class="validate" type="number" min="0" max="1" step="any" value={learningRate.bind.toString}
               oninput={_:Event => dataChanged()} required={true} />
        <label for="learning-rate" data:data-error="Learning rate must be between 0 and 1">Learning Rate</label>
      </div>

      <div class="input-field col s3 offset-s2">
        <input id="discount-factor" class="validate" type="number" min="0" max="1" step="any" value={discountFactor.bind.toString}
               oninput={_:Event => dataChanged()} required={true} />
        <label for="discount-factor" data:data-error="Discount factor must be between 0 and 1">Forgetting Factor</label>
      </div>

    </div>
  }

  private def dataChanged(): Unit = {
    val learningRateElem = getElem[html.Input]("learning-rate")
    val discountFactorElem = getElem[html.Input]("discount-factor")

    if (learningRateElem.validity.valid) {
      learningRate := learningRateElem.value.toDouble
      qTable.learningRate = learningRate.get
    }

    if (discountFactorElem.validity.valid) {
      discountFactor := discountFactorElem.value.toDouble
      qTable.discountFactor = discountFactor.get
    }
  }

  private def getCheckBoxID(space: QStateSpace[O]): String = "q-enable-" + space.name

  private def checkBoxToggled(space: QStateSpace[O]): Unit = {
    val idx = spaces.indexOf(space)
    val checkBox = getElem[html.Input](getCheckBoxID(space))

    spacesEnabled.get(idx) = (space, checkBox.checked)
  }
}

object QTableController {

  def builder[O,A](
    numActions: Int, actionMap: (Int) => A,
    spaces: QStateSpace[O]*
  ): ModelController.Builder[Agent[O,A]] = {

    "Tabular Q Learning" -> (() => new QTableController(numActions, actionMap, spaces.toArray))
  }
}
