package rlp.models

import com.thoughtworks.binding.Binding.{Var, Vars}
import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.{Event, html}
import org.scalajs.dom.raw.HTMLElement
import rlp._
import rlp.agent.{Agent, QStateSpace, QTableAgent}
import upickle.Js

class QTableModel[O, A](
  environment: String,
  numActions: Int,
  actionMap: (Int) => A,
  params: Array[ModelParam[QStateSpace[O]]]
) extends Model[Agent[O, A]](environment, QTableModel.name) {

  private val learningRate = Var(0.1)
  private val discountFactor = Var(0.9)

  private var qTable: QTableAgent = _

  private val paramSelector = new ParamSelector(params)
  private val paramBindings = paramSelector.paramBindings

  @dom
  override lazy val modelBuilder: Binding[HTMLElement] = {
    <div class="row">

      <h5 class="col offset-s1 s11 light">Q Table Inputs</h5>
      <div class="col s12"> { paramSelector.builder.bind } </div>
      <h6 class="col offset-s3 s6 center-align">
        Table Size: {
          (
            for {
              (param, enabled) <- paramBindings.bind
              if enabled
            } yield param.value.size
          ).product.toString
        }
      </h6>
    </div>
  }


  override def buildAgent(): Agent[O, A] = {
    val qSpaces = for ((param, enabled) <- paramBindings.get; if enabled) yield param.value
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

  override def resetAgent(): Unit = {
    super.resetAgent()
    agent.reset()
  }

  override def cloneBuildFrom(controller: Model[Agent[O,A]]): Unit = {

    // Require the controller to be a QTableModel
    val that = controller.asInstanceOf[QTableModel[O,A]]

    paramBindings.get.clear()
    paramBindings.get ++= that.paramBindings.get
  }

  override protected def storeBuild(): Js.Obj = {
    Js.Obj(
      "params" -> paramSelector.store()
    )
  }

  override protected def loadBuild(data: Js.Value): Unit = {
    paramSelector.load(data.obj("params"))
  }

  override protected def storeAgent(): Js.Value = qTable.store()

  override protected def loadAgent(data: Js.Value): Unit = qTable.load(data)
}

object QTableModel {

  val name = "Q Table"

  def builder[O,A](
    environment: String,
    numActions: Int, actionMap: (Int) => A,
    params: ModelParam[QStateSpace[O]]*
  ): Model.Builder[Agent[O,A]] = {

    name -> (() => new QTableModel(environment, numActions, actionMap, params.toArray))
  }
}

