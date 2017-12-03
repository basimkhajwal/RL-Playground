package rlp.models

import com.thoughtworks.binding.Binding.{Constant, Constants, Var, Vars}
import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.{Event, html}
import org.scalajs.dom.html.Div
import org.scalajs.dom.raw.HTMLElement
import rlp.agent.QNetworkAgent.QNetworkSpace
import rlp.agent.{Agent, QNetworkAgent}
import rlp.ai.ActivationFunction.{Linear, ReLU, Sigmoid}
import rlp.ai.{ActivationFunction, NeuralNetwork}
import rlp._
import rlp.utils.SelectHandler

class QNetworkModel[S,A](
  numActions: Int,
  actionMap: Int => A,
  params: Array[ModelParam[QNetworkSpace[S]]]
) extends Model[Agent[S,A]](QNetworkModel.name){

  type Param = ModelParam[QNetworkSpace[S]]

  private val activationFunctions: List[(String, ActivationFunction)] = List(
    "ReLU" -> ReLU,
    "Sigmoid" -> Sigmoid,
    "Linear" -> Linear
  )
  private val outputActivation = Sigmoid
  private val maxHiddenLayers = 5

  private val paramSelector = new ParamSelector(params)
  private val paramsEnabled = paramSelector.paramsEnabled

  private var qNetwork: QNetworkAgent = _

  class LayerDef(
    val size: Var[Int] = Var(10),
    val activation: Var[ActivationFunction] = Var(ReLU),
    val index: Var[Int] = Var(0)
  ) {

    private val activationSelector = new SelectHandler(
      "Activation Function", activationFunctions.map(_._1), Constant(false)
    )
    activationSelector.selectedIndex := activationFunctions.indexWhere(_._2 == activation.get)

    private def activationChanged(activationIdx: Int): Unit = {
      activation := activationFunctions(activationIdx)._2
    }

    private def inputChanged(inputID: String): Unit = {
      val inputElem = getElem[html.Input](inputID)
      if (inputElem.validity.valid) {
        size := inputElem.value.toInt
      }
    }

    @dom
    lazy val handler: Binding[Div] = {
      val inputID = getGUID("input-size")

      <div class="layer-definition row">
        <span class="col s3">{s"Hidden Layer ${index.bind+1}"}</span>

        <div class="input-field col s4">
          <input id={inputID}
                 value={size.bind.toString} onchange={_:Event => inputChanged(inputID)}
                 class="validate" type="number" min="1" max="100" step="1" />
          <label for={inputID} class="active" data:data-error="Enter size in range 1-100">Layer Size</label>
        </div>

        <div class="col s4">
          { activationSelector.handler.bind }
        </div>

        {
          activationChanged(activationSelector.selectedIndex.bind)
          ""
        }

        <div class="col s1">
          <a class="btn-flat grey waves-effect waves-light"
             onclick={_:Event => deleteLayer(this)}>
            <i class="material-icons small">delete</i>
          </a>
        </div>
      </div>
    }
  }

  object LayerDef {
    def apply(size: Int = 10, activation: ActivationFunction = ReLU, index: Int = 0): LayerDef = {
      new LayerDef(Var(size), Var(activation), Var(index))
    }
  }

  private val layerDefinitions = Vars[LayerDef]()

  private def createLayer(): Unit = {
    layerDefinitions.get += LayerDef(index = layerDefinitions.get.length)
  }

  private def deleteLayer(layer: LayerDef): Unit = {
    val idx = layerDefinitions.get.indexOf(layer)
    if (idx >= 0) {
      layerDefinitions.get.remove(idx)
      for (i <- idx until layerDefinitions.get.length) {
        layerDefinitions.get(i).index := layerDefinitions.get(i).index.get - 1
      }
    }
  }

  @dom
  override lazy val modelBuilder: Binding[HTMLElement] = {
    <div class="row">
      <h5 class="col s11 offset-s1 light">Layer Definitions</h5>
      <div id="layer-definitions" class="col s10 offset-s1">
        <div class="layer-definition" id="input-layer">
          <span class="center-align">Input Layer</span>
          <br />
          { paramSelector.builder.bind }
        </div>

        {
          for (layer <- layerDefinitions) yield {
            layer.handler.bind
          }
        }

        <div id="create-layer">
          <a onclick={_:Event => createLayer()}
             class={"btn" + (if (layerDefinitions.length.bind >= maxHiddenLayers) " disabled" else "")}>
            <i class="material-icons left">add_circle_outline</i> Add Hidden Layer
          </a>
        </div>

        <div class="layer-definition" id="output-layer">
          <span class="center-align">{ s"Output Layer - $numActions neurons"} </span>
        </div>
      </div>
    </div>
  }

  override def buildAgent(): Agent[S, A] = {

    val inputs = for ((param, enabled) <- paramsEnabled.get; if enabled) yield param.value

    val hiddenLayerSizes = layerDefinitions.get.map(_.size.get).toArray
    val hiddenLayerActivations = layerDefinitions.get.map(_.activation.get).toArray

    val layerSizes = Array(inputs.map(_.size).sum) ++ hiddenLayerSizes ++ Array(numActions)
    val layerActivations = hiddenLayerActivations ++ Array(outputActivation)

    qNetwork = new QNetworkAgent(new NeuralNetwork(layerSizes, layerActivations))
    qNetwork.reset()

    QNetworkAgent.build(qNetwork, actionMap, inputs)
  }

  override def cloneBuildFrom(that: Model[Agent[S,A]]): Unit = {

    val controller = that.asInstanceOf[QNetworkModel[S,A]]

    paramsEnabled.get.clear()
    paramsEnabled.get ++= controller.paramsEnabled.get

    layerDefinitions.get.clear()
    layerDefinitions.get ++= controller.layerDefinitions.get.map(
      l => LayerDef(l.size.get, l.activation.get, l.index.get)
    )
  }

  override def resetAgent(): Unit = {
    qNetwork.reset()
  }
}

object QNetworkModel {

  val name = "Q Network"

  def builder[S,A](numActions: Int, actionMap: Int => A, params: ModelParam[QNetworkSpace[S]]*): Model.Builder[Agent[S,A]] = {
    name -> (() => new QNetworkModel(numActions, actionMap, params.toArray))
  }
}
