package rlp.presenters

import com.thoughtworks.binding.Binding.Var
import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.raw.HTMLElement
import rlp._
import rlp.agent.{Agent, QTableAgent}
import rlp.agent.QTableAgent.QStateSpace
import rlp.ui.NumericInputHandler
import ujson.Js

/**
  * The presenter for the QTableAgent
  *
  * @param environment
  * @param numActions
  * @param actionMap
  * @param params
  * @tparam O
  * @tparam A
  */
class QTablePresenter[O, A](
  environment: String,
  numActions: Int,
  actionMap: (Int) => A,
  params: Array[AgentParam[QStateSpace[O]]]
) extends AgentPresenter[Agent[O, A]](environment, QTablePresenter.name) {

  // Train time parameters
  private val learningRate = Var(0.1)
  private val discountFactor = Var(0.9)
  private val explorationEpsilon = Var(0.1)

  private var qTable: QTableAgent = _

  private val paramSelector = new ParamSelector(params)
  private val paramBindings = paramSelector.paramBindings

  @dom
  override lazy val agentBuilder: Binding[HTMLElement] = {
    <div class="row content-section">

      <div class="col s12">
        <h5>Q Table Inputs</h5>
        <div class="divider"></div>
      </div>

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

  override def load(agentStore: AgentStore): Unit = {
    super.load(agentStore)

    /* Reset from imported file */
    learningRate := qTable.learningRate
    discountFactor := qTable.discountFactor
    explorationEpsilon := qTable.explorationEpsilon
  }

  @dom
  override lazy val agentViewer: Binding[HTMLElement] = {
    <div>

      { paramSelector.viewer.bind }

      <div class="content-section">
        <h5>Q Table Parameters</h5>
        <div class="divider"></div>

        <div class="row">
          <div class="col s2 offset-s2">
            { new NumericInputHandler("Learning Rate", learningRate, 0, 1).content.bind }
          </div>

          <div class="col s2 offset-s1">
            { new NumericInputHandler("Discount Factor", discountFactor, 0, 1).content.bind }
          </div>

          <div class="col s2 offset-s1">
            { new NumericInputHandler("Exploration Epsilon", explorationEpsilon, 0, 1).content.bind }
          </div>
        </div>
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

    viewChanged()
  }

  override def resetAgent(): Unit = {
    super.resetAgent()
    agent.reset()
  }

  override def cloneBuildFrom(controller: AgentPresenter[Agent[O,A]]): Unit = {

    // Require the controller to be a QTablePresenter
    val that = controller.asInstanceOf[QTablePresenter[O,A]]

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

object QTablePresenter {

  val name = "Q Table"

  def builder[O,A](
    environment: String,
    numActions: Int, actionMap: (Int) => A,
    params: AgentParam[QStateSpace[O]]*
  ): AgentPresenter.Builder[Agent[O,A]] = {

    name -> (() => new QTablePresenter(environment, numActions, actionMap, params.toArray))
  }
}

