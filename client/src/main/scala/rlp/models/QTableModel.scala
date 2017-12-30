package rlp.models

import com.thoughtworks.binding.Binding.Var
import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.raw.HTMLElement
import rlp._
import rlp.agent.{Agent, QStateSpace, QTableAgent}
import rlp.utils.NumericInputHandler
import upickle.Js

class QTableModel[O, A](
  environment: String,
  numActions: Int,
  actionMap: (Int) => A,
  params: Array[ModelParam[QStateSpace[O]]]
) extends Model[Agent[O, A]](environment, QTableModel.name) {

  private val learningRate = Var(0.1)
  private val discountFactor = Var(0.9)
  private val explorationEpsilon = Var(0.1)

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

  override def load(modelStore: ModelStore): Unit = {
    super.load(modelStore)

    /* Reset from imported file */
    learningRate := qTable.learningRate
    discountFactor := qTable.discountFactor
    explorationEpsilon := qTable.explorationEpsilon
  }

  @dom
  override lazy val modelViewer: Binding[HTMLElement] = {
    <div>

      <div class="col s10 offset-s1">{paramSelector.viewer.bind}</div>

      <h5 class="col offset-s1 s11">Q Table Parameters</h5>

      <div class="col s2 offset-s2">
        { new NumericInputHandler("Learning Rate", learningRate, 0, 1).content.bind }
      </div>

      <div class="col s2 offset-s1">
        { new NumericInputHandler("Discount Factor", discountFactor, 0, 1).content.bind }
      </div>

      <div class="col s2 offset-s1">
        { new NumericInputHandler("Exploration Epsilon", explorationEpsilon, 0, 1).content.bind }
      </div>

      {
        dataChanged(learningRate.bind, discountFactor.bind, explorationEpsilon.bind)
        ""
      }
    </div>
  }

  private def dataChanged(lr: Double, df: Double, e: Double): Unit = {
    qTable.learningRate = lr
    qTable.discountFactor = df
    qTable.explorationEpsilon = e
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

