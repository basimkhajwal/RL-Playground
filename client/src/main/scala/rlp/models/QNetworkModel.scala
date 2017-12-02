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

  val numStates = params.map(_.value.size).sum

  private val paramSelector = new ParamSelector(params)
  private val paramsEnabled = paramSelector.paramsEnabled

  private var qNetwork: QNetworkAgent = _

  case class LayerDef(
    size: Var[Int] = Var(10),
    activation: Var[ActivationFunction] = Var(ReLU),
    index: Var[Int] = Var(0)
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
      val idx = index.bind
      val inputID = getGUID("input-size")

      <div class="layer-definition row">
        <span class="col s3">Hidden Layer {idx.toString}</span>

        <div class="input-field col s4">
          <input id={inputID}
                 value={size.bind.toString} onchange={_:Event => inputChanged(inputID)}
                 class="validate" type="number" min="1" max="100" step="1" />
          <label for={inputID} data:data-error="Enter size in range 1-100">Layer Size</label>
        </div>

        <div class="col s4">
          {
            activationChanged(activationSelector.selectedIndex.bind)
            activationSelector.handler.bind
          }
        </div>

        <div class="col s1">
          <a class="btn-flat grey waves-effect waves-light"
             onclick={_:Event => deleteLayer(this)}>
            <i class="material-icons small">delete</i>
          </a>
        </div>
      </div>
    }
  }

  private val activationFunctions: List[(String, ActivationFunction)] = List(
    "ReLU" -> ReLU,
    "Sigmoid" -> Sigmoid,
    "Linear" -> Linear
  )

  private val maxHiddenLayers = 5

  private val layerDefinitions = Vars[LayerDef]()

  private def createLayer(): Unit = {
    layerDefinitions.get += LayerDef(index = Var(layerDefinitions.get.length))
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
    val network = new NeuralNetwork(Array(numStates, 10, numActions), Array(Sigmoid, Sigmoid))
    network.randomiseWeights(-0.5, 0.5)

    qNetwork = new QNetworkAgent(network)

    QNetworkAgent.build(qNetwork, actionMap, params.map(_.value))
  }

  override def cloneBuildFrom(that: Model[Agent[S,A]]): Unit = {

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
