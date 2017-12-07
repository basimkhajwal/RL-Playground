package rlp.models

import scala.reflect.runtime.universe.TypeTag
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
import rlp.ai.optimizers.{Adam, NetworkOptimizer, RMSProp, SGDMomentum}
import rlp.utils.SelectHandler

import scala.reflect.ClassTag

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
  private val inputParams = for ((param, enabled) <- paramsEnabled; if enabled) yield param

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

      <div class="layer-definition row valign-wrapper">
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
          <a class="btn-floating red lighten-2 waves-effect waves-light"
             onclick={_:Event => deleteLayer(this)}>
            <i class="material-icons">delete</i>
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
        <div class="layer-definition row" id="input-layer">
          <span class="col s3">Input Layer</span>
          <span class="col s4">{inputParams.bind.map(_.value.size).sum.toString} neurons</span>
          <div class="col s12">
            { paramSelector.builder.bind }
          </div>
        </div>

        {
          for (layer <- layerDefinitions) yield {
            layer.handler.bind
          }
        }

        <div class="layer-definition row" id="output-layer">
          <span class="col s3">Output Layer</span>
          <span class="col s4">{s"$numActions neurons"}</span>
          <span class="col s4">
            { activationFunctions.find(_._2 == outputActivation).map(_._1).getOrElse("")}
          </span>
        </div>
      </div>

      <div class="col s12" id="create-layer">
        <a onclick={_:Event => createLayer()}
           class={"btn light-green" + (if (layerDefinitions.length.bind >= maxHiddenLayers) " disabled" else "")}>
          <i class="material-icons left">add_circle_outline</i> Add Hidden Layer
        </a>
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

  case class OptimiserParam(
    name: String,
    extractor: NetworkOptimizer => Double,
    min: Double = Double.NegativeInfinity,
    max: Double = Double.PositiveInfinity,
  )

  case class OptimiserBuilder(
    name: String,
    params: List[OptimiserParam],
    paramConstructor: Array[Double] => NetworkOptimizer,
    default: NetworkOptimizer,
    isInstance: NetworkOptimizer => Boolean
  ) {

    val dirty: Var[Boolean] = Var(false)

    val initialValues: Array[Double] = params.map(_.extractor(default)).toArray
    val paramDirty = new Array[Boolean](initialValues.size)
    val paramValues: Vars[Double] = Vars(initialValues:_*)

    def selectOptimiser(opt: NetworkOptimizer): Unit = {
      if (isInstance(opt)) {
        for (i <- params.indices) {
          initialValues(i) = params(i).extractor(opt)
          paramValues.get(i) = initialValues(i)
          paramDirty(i) = false
        }
        dirty := false
      } else {
        selectOptimiser(default)
      }
    }

    def construct(): NetworkOptimizer = {
      paramConstructor(paramValues.get.toArray)
    }

    private def paramUpdated(pid: String, idx: Int): Unit = {
      val elem = getElem[html.Input](pid)

      if (elem.validity.valid) {
        val value = elem.value.toDouble
        paramValues.get(idx) = value

        if ( (value != initialValues(idx)) ^ paramDirty(idx) ) {
          paramDirty(idx) = !paramDirty(idx)
          dirty := paramDirty.reduce(_ || _)
        }
      }
    }

    @dom
    lazy val handler: Binding[html.Div] = {
      <div>
        {
          val indexed = Constants(params.zipWithIndex :_*)
          val values = paramValues.bind

          for ((param, idx) <- indexed) yield {
            val pid = getGUID(param.name)

            <div class="input-field">
              <input id={pid} class="validate" value={values(idx).toString}
                onchange={_:Event => paramUpdated(pid, idx)}
                type="number" min={param.min.toString} max={param.max.toString} step="any"
              />
              <label for={pid} class="active" data:data-error={s"${param.name} must be between ${param.min} and ${param.max}"}>
                {param.name}
              </label>
            </div>
          }
        }
      </div>
    }
  }

  lazy val optimisers = Array(
    OptimiserBuilder(
      "Stochastic Gradient Descent",
      List(
        OptimiserParam("Learning Rate", { _.asInstanceOf[SGDMomentum].learningRate }, min = 0),
        OptimiserParam("Momentum", { _.asInstanceOf[SGDMomentum].momentum }, min = 0),
        OptimiserParam("Decay", { _.asInstanceOf[SGDMomentum].decay }, min = 0),
      ),
      { ps => new SGDMomentum(qNetwork.network, learningRate = ps(0), momentum = ps(1), decay = ps(2)) },
      new SGDMomentum(qNetwork.network),
      { _.isInstanceOf[SGDMomentum] }
    ),
    OptimiserBuilder(
      "ADAM",
      List(
        OptimiserParam("Learning Rate", { _.asInstanceOf[Adam].learningRate }, min = 0),
        OptimiserParam("Beta 1", { _.asInstanceOf[Adam].beta1 }, min = 0, max = 1),
        OptimiserParam("Beta 2", { _.asInstanceOf[Adam].beta2 }, min = 0, max = 1),
        OptimiserParam("Decay", { _.asInstanceOf[Adam].decay }, min = 0),
      ),
      { ps => new Adam(qNetwork.network, learningRate = ps(0), beta1 = ps(1), beta2 = ps(2), decay = ps(3)) },
      new Adam(qNetwork.network),
      { _.isInstanceOf[Adam] }
    ),
    OptimiserBuilder(
      "RMSProp",
      List(
        OptimiserParam("Learning Rate", { _.asInstanceOf[RMSProp].learningRate }, min = 0),
        OptimiserParam("Rho", { _.asInstanceOf[RMSProp].rho }, min = 0),
        OptimiserParam("Decay", { _.asInstanceOf[RMSProp].decay }, min = 0),
      ),
      { ps => new RMSProp(qNetwork.network, learningRate = ps(0), rho = ps(1), decay = ps(2)) },
      new RMSProp(qNetwork.network),
      { _.isInstanceOf[RMSProp] }
    )
  )

  @dom
  override lazy val modelViewer: Binding[HTMLElement] = {

    val optimiserSelect = new SelectHandler("Optimiser", optimisers.map(_.name), Constant(false))

    def currentOptimiserIndex: Int = {
      optimisers.indexWhere(_.isInstance(qNetwork.optimiser))
    }

    optimiserSelect.selectedIndex := currentOptimiserIndex

    val selectedOptimiser = Binding { optimisers(optimiserSelect.selectedIndex.bind) }

    val dirty = Binding {
      currentOptimiserIndex != optimiserSelect.selectedIndex.bind || selectedOptimiser.bind.dirty.bind
    }

    val buttonClasses = Binding { "btn" + (if (dirty.bind) "" else " disabled") }

    def undoChanges(): Unit = {
      optimisers(currentOptimiserIndex).selectOptimiser(qNetwork.optimiser)
      optimiserSelect.selectedIndex := currentOptimiserIndex
    }

    @dom
    def updateOptimiser(): Unit = {
      val optimiserBuilder = selectedOptimiser.bind
      qNetwork.optimiser = optimiserBuilder.construct()
      undoChanges()
    }

    <div class="row">

      <div class="col s3 offset-s1">
        { optimiserSelect.handler.bind }
      </div>

      <div class="col s3 offset-s1">
        <a class={buttonClasses.bind} onclick={_:Event => undoChanges()}>Undo Changes</a>
      </div>

      <div class="col s3">
        <a class={buttonClasses.bind} onclick={_:Event => updateOptimiser()}>Update Optimiser</a>
      </div>

      <div class="col s10 offset-s1">
        { optimisers(optimiserSelect.selectedIndex.bind).handler.bind }
      </div>

    </div>
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
