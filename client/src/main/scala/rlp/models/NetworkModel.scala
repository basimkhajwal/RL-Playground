package rlp.models

import com.thoughtworks.binding.Binding.{Constant, Constants, Var, Vars}
import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.{Event, html}
import rlp.agent.Agent
import rlp.ai.ActivationFunction.{Linear, ReLU, Sigmoid}
import rlp.ai.{ActivationFunction, NeuralNetwork}
import rlp.ai.optimizers.{Adam, NetworkOptimizer, RMSProp, SGDMomentum}
import rlp.utils.SelectHandler
import rlp._

abstract class NetworkModel[S,A,P](
  name: String,
  params: Array[ModelParam[P]],
  paramSize: P => Int,
  outputSize: Int
) extends Model[Agent[S,A]](name) {

  protected val activationFunctions: List[(String, ActivationFunction)] = List(
    "ReLU" -> ReLU,
    "Sigmoid" -> Sigmoid,
    "Linear" -> Linear
  )

  protected val outputActivation = Sigmoid

  protected val maxHiddenLayers = 5

  private val paramSelector = new ParamSelector(params)
  protected val paramBindings = paramSelector.paramBindings
  private val paramsEnabled = for ((param, enabled) <- paramBindings; if enabled) yield param

  class LayerDef(
    val size: Var[Int] = Var(10),
    val activation: Var[ActivationFunction] = Var(ReLU),
    val index: Var[Int] = Var(0)
  ) {

    private val activationSelector = new SelectHandler(
      "Activation Function", activationFunctions.map(_._1)
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
    lazy val handler: Binding[html.Div] = {
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
  protected final lazy val networkBuilder: Binding[html.Div] = {
    <div class="row">
      <h5 class="col s11 offset-s1 light">Layer Definitions</h5>
      <div id="layer-definitions" class="col s10 offset-s1">
        <div class="layer-definition row" id="input-layer">
          <span class="col s3">Input Layer</span>
          <span class="col s4">{paramsEnabled.bind.map(p => paramSize(p.value)).sum.toString} neurons</span>
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
          <span class="col s4">{s"$outputSize neurons"}</span>
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

  protected final def buildNetwork(): NeuralNetwork = {

    val inputSizes = for ((param, enabled) <- paramBindings.get; if enabled) yield paramSize(param.value)
    val inputSize = inputSizes.sum

    val hiddenLayerSizes = layerDefinitions.get.map(_.size.get).toArray
    val hiddenLayerActivations = layerDefinitions.get.map(_.activation.get).toArray

    val layerSizes = Array(inputSize) ++ hiddenLayerSizes ++ Array(outputSize)
    val layerActivations = hiddenLayerActivations ++ Array(outputActivation)

    new NeuralNetwork(layerSizes, layerActivations)
  }

  override def cloneBuildFrom(that: Model[Agent[S, A]]): Unit = {
    val controller = that.asInstanceOf[NetworkModel[S,A,P]]

    layerDefinitions.get.clear()
    layerDefinitions.get ++= controller.layerDefinitions.get.map(
      l => LayerDef(l.size.get, l.activation.get, l.index.get)
    )
  }

  protected def getNetwork(): NeuralNetwork

  protected def getOptimiser(): NetworkOptimizer

  protected def setOptimiser(optimiser: NetworkOptimizer): Unit

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
      <div class="optimiser-input-container">
        {
        val indexed = Constants(params.zipWithIndex :_*)
        val values = paramValues.bind

        for ((param, idx) <- indexed) yield {
          val pid = getGUID(param.name)

          <div class="input-field optimiser-input">
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

  @dom
  private def networkVisualisation(network: NeuralNetwork): Binding[html.Div] = {

    val maxWidth = 800

    val maxLayerWidth = 200
    val layerWidth = Math.min(maxLayerWidth, maxWidth / network.numLayers)

    val headerHeight = 100
    val nodeHeight = 300

    val width = layerWidth * network.numLayers
    val height = headerHeight + nodeHeight

    val nodeRadius = 10
    val nodeSpacing = 10
    val maxNodes = 10

    def layerSize(layer: Int): Int = Math.min(maxNodes, network.layerSizes(layer))

    def getNodePosition(layer: Int, idx: Int): (Int, Int) = {

      val n = layerSize(layer)

      val layerHeight = 2 * nodeRadius * n + (n - 1) * nodeSpacing
      val layerOffset = headerHeight + (nodeHeight - layerHeight) / 2

      val x = layer * layerWidth + (layerWidth - 2 * nodeRadius) / 2

      val y = layerOffset + (2 * nodeRadius + nodeSpacing) * idx

      (x + nodeRadius, y + nodeRadius)
    }

    <div class="network-visualisation">
      <svg data:width={width.toString} data:height={height.toString}>

        <!-- Network Nodes -->
        {
        for {
          layer <- Constants(0 until network.numLayers :_*)
          idx <- Constants(0 until layerSize(layer) :_*)
        } yield {
          val (x,y) = getNodePosition(layer, idx)
            <circle
            data:cx={x.toString}
            data:cy={y.toString}
            data:fill="black"
            data:r={nodeRadius.toString}
            />
        }
        }

        <!-- Network Connections -->
        {
        for {
          layerA <- Constants(0 until (network.numLayers - 1): _*)
          layerB = layerA + 1
          idxA <- Constants(0 until layerSize(layerA): _*)
          idxB <- Constants(0 until layerSize(layerB): _*)
        } yield {

          val (x1, y1) = getNodePosition(layerA, idxA)
          val (x2, y2) = getNodePosition(layerB, idxB)

            <line
            data:x1={x1.toString} data:x2={x2.toString}
            data:y1={y1.toString} data:y2={y2.toString}
            data:stroke-width="2" data:stroke="rgba(0,0,0,0.5)"/>
        }
        }

        <!-- Network Labels -->
        {
        {
          for (layer <- Constants(0 until network.numLayers :_*)) yield {

            val x = layer * layerWidth + layerWidth / 2

            <g data:text-anchor="middle" data:style="font-size:12px; font-weight:300;">
              <text data:x={x.toString} data:y="15" data:style="font-size:14px; font-weight:400;">
                {
                if (layer == 0) "Input Layer"
                else if (layer == network.numLayers-1) "Output Layer"
                else "Hidden Layer " + layer
                }
              </text>

              <text data:x={x.toString} data:y="40">
                { network.layerSizes(layer) + " neurons" }
              </text>

              <text data:x={x.toString} data:y="60">
                {
                if (layer == 0) ""
                else {
                  activationFunctions.find(_._2 == network.activationFunctions(layer-1)) match {
                    case Some((name, _)) => name + " activation"
                    case None => "Unknown activation"
                  }
                }
                }
              </text>
            </g>
          }
        }
        }
      </svg>
    </div>
  }

  lazy val optimisers = Array(
    OptimiserBuilder(
      "Stochastic Gradient Descent",
      List(
        OptimiserParam("Learning Rate", { _.asInstanceOf[SGDMomentum].learningRate }, min = 0),
        OptimiserParam("Momentum", { _.asInstanceOf[SGDMomentum].momentum }, min = 0),
        OptimiserParam("Decay", { _.asInstanceOf[SGDMomentum].decay }, min = 0),
      ),
      { ps => new SGDMomentum(getNetwork(), learningRate = ps(0), momentum = ps(1), decay = ps(2)) },
      new SGDMomentum(getNetwork()),
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
      { ps => new Adam(getNetwork(), learningRate = ps(0), beta1 = ps(1), beta2 = ps(2), decay = ps(3)) },
      new Adam(getNetwork()),
      { _.isInstanceOf[Adam] }
    ),
    OptimiserBuilder(
      "RMSProp",
      List(
        OptimiserParam("Learning Rate", { _.asInstanceOf[RMSProp].learningRate }, min = 0),
        OptimiserParam("Rho", { _.asInstanceOf[RMSProp].rho }, min = 0),
        OptimiserParam("Decay", { _.asInstanceOf[RMSProp].decay }, min = 0),
      ),
      { ps => new RMSProp(getNetwork(), learningRate = ps(0), rho = ps(1), decay = ps(2)) },
      new RMSProp(getNetwork()),
      { _.isInstanceOf[RMSProp] }
    )
  )

  @dom
  protected final lazy val networkViewer: Binding[html.Div] = {
    val optimiserSelect = new SelectHandler("Optimiser", optimisers.map(_.name), Constant(false))

    def currentOptimiserIndex: Int = {
      optimisers.indexWhere(_.isInstance(getOptimiser()))
    }

    optimiserSelect.selectedIndex := currentOptimiserIndex

    val currentOptimiserBinding = Var(getOptimiser())

    val dirty = Binding {
      val currentOptimiser = currentOptimiserBinding.bind
      val optIdx = optimisers.indexWhere(_.isInstance(currentOptimiser))
      val selectedOptimiser = optimisers(optimiserSelect.selectedIndex.bind)
      optIdx != optimiserSelect.selectedIndex.bind || selectedOptimiser.dirty.bind
    }

    val buttonClasses = Binding {
      "btn" + (if (dirty.bind) "" else " disabled")
    }

    def undoChanges(): Unit = {
      val idx = currentOptimiserIndex
      currentOptimiserBinding := getOptimiser()
      optimisers(idx).selectOptimiser(currentOptimiserBinding.get)
      optimiserSelect.selectedIndex := idx
    }

    def updateOptimiser(): Unit = {
      val optimiserBuilder = optimisers(optimiserSelect.selectedIndex.get)
      setOptimiser(optimiserBuilder.construct())
      undoChanges()
    }

    <div class="row">

      <div class="col s10 offset-s1">
        <h5>Network Structure</h5>
      </div>

      <div class="row col s10 offset-s1">
        { networkVisualisation(getNetwork()).bind }
      </div>

      <div class="col s10 offset-s1">
        <h5>Optimiser Select</h5>
      </div>

      <div class="row col s10 offset-s1 valign-wrapper">
        <div class="col s3">
          { optimiserSelect.handler.bind }
        </div>

        <div class="col s3 offset-s1">
          <a class={buttonClasses.bind} onclick={_:Event => undoChanges()}>Undo Changes</a>
        </div>

        <div class="col s3">
          <a class={buttonClasses.bind} onclick={_:Event => updateOptimiser()}>Update Optimiser</a>
        </div>
      </div>

      <div class="col s10 offset-s1">
        { optimisers(optimiserSelect.selectedIndex.bind).handler.bind }
      </div>

    </div>
  }
}
