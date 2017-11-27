package rlp.models

import com.thoughtworks.binding.Binding.{Var, Vars}
import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.{Event, html}
import org.scalajs.dom.raw.HTMLElement
import rlp._
import rlp.agent.{Agent, QStateSpace, QTableAgent}

class QTableModel[O, A](
  numActions: Int, actionMap: (Int) => A, params: Array[QModelParam[O]]
) extends Model[Agent[O, A]](QTableModel.name) {

  private val paramsEnabled: Vars[(QModelParam[O], Boolean)] = Vars(params.map(s => (s, s.defaultEnabled)) :_ *)

  private val learningRate = Var(0.1)
  private val discountFactor = Var(0.9)

  private var qTable: QTableAgent = _

  @dom
  override lazy val modelBuilder: Binding[HTMLElement] = {
    <div class="row">

      <h5 class="col offset-s1 s11 light">Q Table Inputs</h5>
      <div class="col s12" id="q-checkbox-container">
        {
        for ((param, enabled) <- paramsEnabled) yield {
          <div class="q-table-checkbox">
            <input type="checkbox" id={getCheckBoxID(param)}
                   onchange={ _:Event => checkBoxToggled(param)}
                   checked={enabled}
            />
            <label for={getCheckBoxID(param)}>{param.name}</label>
          </div>
        }
        }
      </div>
    </div>
  }


  override def buildAgent(): Agent[O, A] = {
    val qSpaces = for ((param, enabled) <- paramsEnabled.get; if enabled) yield param.space
    val (qAgent, agent) = QTableAgent.build(numActions, actionMap, qSpaces)

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

  private def getCheckBoxID(param: QModelParam[O]): String = "q-enable-" + param.name

  private def checkBoxToggled(param: QModelParam[O]): Unit = {
    val idx = params.indexOf(param)
    val checkBox = getElem[html.Input](getCheckBoxID(param))

    paramsEnabled.get(idx) = (param, checkBox.checked)
  }

  override def resetAgent(): Unit = {
    agent.reset()
  }

  override def cloneBuild(): QTableModel[O,A] = {
    val clone = new QTableModel(numActions, actionMap, params)

    clone.paramsEnabled.get.clear()
    clone.paramsEnabled.get ++= paramsEnabled.get

    clone
  }

  override protected def _duplicate(): QTableModel[O,A] = {
    val clone = cloneBuild()
    clone.agent // Force build agent

    // Copy across learnt q values
    for (i <- qTable.table.indices) {
      clone.qTable.table(i) = qTable.table(i)
    }

    clone
  }
}

object QTableModel {

  val name = "Q Table"

  def builder[O,A](
    numActions: Int, actionMap: (Int) => A,
    params: QModelParam[O]*
  ): Model.Builder[Agent[O,A]] = {

    name -> (() => new QTableModel(numActions, actionMap, params.toArray))
  }
}

case class QModelParam[T](
  name: String,
  space: QStateSpace[T],
  defaultEnabled: Boolean = true
)
