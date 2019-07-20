package rlp.presenters

import com.thoughtworks.binding.Binding.{Constant, Constants, Var, Vars}
import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.{Event, html}
import rlp.agent.Agent
import rlp.ai.ActivationFunction.{Linear, ReLU, Sigmoid, TanH}
import rlp.ai.{ActivationFunction, NeuralNetwork}
import rlp.ai.optimizers.{Adam, NetworkOptimizer, RMSProp, SGDMomentum}
import rlp._
import rlp.ui.{NumericInputHandler, SelectHandler}
import ujson.Js

import scala.scalajs.js

/**
  * The agent presenter base class for the neural network agents:
  * QNetworkPresenter and PolicyNetworkPresenter
  *
  * Defines the build definition for handling multiple hidden layers
  * with different neuron counts and activation functions.
  *
  * Also has an SVG neural network visualisation and optimiser
  * selecting user interfaces.
  *
  * @param environment The environment for this agent presenter
  * @param name The type of the agent
  * @param params The parameters for the agent
  * @param paramSize A mapping from a parameter to the number of neuron inputs it requires
  * @param outputSize The number of actions for this agent
  * @tparam S The environment state type
  * @tparam A The action type
  * @tparam P The agent parameter type
  */
abstract class NetworkPresenter[S,A,P](
  environment: String,
  name: String,
  params: Array[AgentParam[P]],
  paramSize: P => Int,
  outputSize: Int
) extends AgentPresenter[Agent[S,A]](environment, name) {

  protected val activationFunctions: List[(String, ActivationFunction)] = List(
    "ReLU" -> ReLU,
    "TanH" -> TanH,
    "Sigmoid" -> Sigmoid,
    "Linear" -> Linear
  )

  // Options for the last layer
  protected val enableSoftMax: Boolean = false
  protected val outputActivation = Linear

  // Default max layers
  protected val maxHiddenLayers = 5

  // Selection bar on top for which parameters to enable
  protected val paramSelector = new ParamSelector(params)
  protected val paramBindings = paramSelector.paramBindings
  private val paramsEnabled = for ((param, enabled) <- paramBindings; if enabled) yield param

  /**
    * UI component representing a single layer in the build definition
    *
    * @param size The layer size in number of neurons
    * @param activation  The activation function
    * @param index The index within the hidden layers
    */
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

    /**
      * The view consisting of row containing:
      * - the layer index label
      * - a NumericInputHandler for the number of neurons
      * - a SelectHandler for the activation function
      * - a delete button
      */
    @dom
    lazy val handler: Binding[html.Div] = {
      <div class="layer-definition row valign-wrapper">
        <span class="col s3">{s"Hidden Layer ${index.bind+1}"}</span>

        <div class="col s4">{ new NumericInputHandler("Layer Size", size, 1, 100).content.bind }</div>

        <div class="col s4">{ activationSelector.handler.bind }</div>

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

    /**
      * Serialise the layer def
      * @return
      */
    def store(): Js.Value = {
      Js.Obj(
        "size" -> Js.Num(size.get),
        "activation" -> Js.Str(activationFunctions.find(_._2 == activation.get).get._1),
        "index" -> Js.Num(index.get)
      )
    }
  }

  object LayerDef {

    /** Convenient constructor for LayerDef */
    def apply(size: Int = 10, activation: ActivationFunction = ReLU, index: Int = 0): LayerDef = {
      new LayerDef(Var(size), Var(activation), Var(index))
    }

    /** De-serialise into a layer def for this presenter */
    def load(data: Js.Value): LayerDef = {

      val keyMap = data.obj

      LayerDef(
        keyMap("size").num.toInt,
        activationFunctions.find(_._1 == keyMap("activation").str).get._2,
        keyMap("index").num.toInt
      )
    }
  }

  // The hidden layer definitions
  private val layerDefinitions = Vars[LayerDef]()

  /**
    * Add a new hidden layer
    */
  private def createLayer(): Unit = {
    layerDefinitions.get += LayerDef(index = layerDefinitions.get.length)
  }

  /**
    * Delete the hidden layer given
    * @param layer
    */
  private def deleteLayer(layer: LayerDef): Unit = {

    /* Find the index of the layer */
    val idx = layerDefinitions.get.indexOf(layer)

    // If the layer is in the definitions, remove it
    if (idx >= 0) {
      layerDefinitions.get.remove(idx)

      // Decrement other layer indices beyond it
      for (i <- idx until layerDefinitions.get.length) {
        layerDefinitions.get(i).index := layerDefinitions.get(i).index.get - 1
      }
    }
  }

  /**
    * The part of the builder constructing the network layers
    */
  @dom
  protected final lazy val networkBuilder: Binding[html.Div] = {
    <div class="content-section">

      <!-- Title -->
      <h5>Layer Definitions</h5>
      <div class="divider"></div>

      <div id="layer-definitions">
        <!-- Input layer -->
        <div class="layer-definition row" id="input-layer">
          <span class="col s3">Input Layer</span>
          <span class="col s4">{paramsEnabled.bind.map(p => paramSize(p.value)).sum.toString} neurons</span>
          <div class="col s12">
            { paramSelector.builder.bind }
          </div>
        </div>

        <!-- Hidden layers -->
        { for (layer <- layerDefinitions) yield layer.handler.bind }

        <!-- Input layer -->
        <div class="layer-definition row" id="output-layer">
          <span class="col s3">Output Layer</span>
          <span class="col s4">{s"$outputSize neurons"}</span>
          <span class="col s4">
            { activationFunctions.find(_._2 == outputActivation).map(_._1).getOrElse("") +
              (if (enableSoftMax) " + softmax" else "")
            }
          </span>
        </div>
      </div>

      <!-- Button for adding a new layer -->
      <div id="create-layer">
        <a onclick={_:Event => createLayer()}
           class={"btn light-green" + (if (layerDefinitions.length.bind >= maxHiddenLayers) " disabled" else "")}>
          <i class="material-icons left">add_circle_outline</i> Add Hidden Layer
        </a>
      </div>
    </div>
  }

  /**
    * Build the neural network from the layers given
    * @return
    */
  protected final def buildNetwork(): NeuralNetwork = {

    // Extract all the enabled input sizes
    val inputSizes =
      for {
        (param, enabled) <- paramBindings.get
        if enabled
      } yield paramSize(param.value)

    // Get the total
    val inputSize = inputSizes.sum

    // Extract hidden layer properties
    val hiddenLayerSizes = layerDefinitions.get.map(_.size.get).toArray
    val hiddenLayerActivations = layerDefinitions.get.map(_.activation.get).toArray

    // Combine input, hidden and output layers
    val layerSizes = Array(inputSize) ++ hiddenLayerSizes ++ Array(outputSize)
    val layerActivations = hiddenLayerActivations ++ Array(outputActivation)
    new NeuralNetwork(layerSizes, layerActivations, enableSoftMax)
  }

  override def cloneBuildFrom(that: AgentPresenter[Agent[S, A]]): Unit = {
    val controller = that.asInstanceOf[NetworkPresenter[S,A,P]]

    layerDefinitions.get.clear()
    layerDefinitions.get ++= controller.layerDefinitions.get.map(
      l => LayerDef(l.size.get, l.activation.get, l.index.get)
    )

    paramBindings.get.clear()
    paramBindings.get ++= controller.paramBindings.get
  }

  override protected def storeBuild(): Js.Value = {
    Js.Obj(
      "layers" -> Js.Arr(layerDefinitions.get.map(_.store()) :_*),
      "params" -> paramSelector.store()
    )
  }

  override protected def loadBuild(build: Js.Value): Unit = {
    val keyMap = build.obj

    val defs = keyMap("layers").arr.map(LayerDef.load)
    layerDefinitions.get.clear()
    layerDefinitions.get.appendAll(defs)

    paramSelector.load(keyMap("params"))
  }

  /* Abstract member functions which the underlying presenter must implement
  * to extract the network, and get/set the optimiser from the agent
  * */

  protected def getNetwork(): NeuralNetwork

  protected def getOptimiser(): NetworkOptimizer

  protected def setOptimiser(optimiser: NetworkOptimizer): Unit

  /**
    * Represents a single parameter for the network optimiser
    *
    * @param name The display name of the optimiser
    * @param extractor Extracting the optimiser's value
    * @param min Minimum bound on value
    * @param max Maximum bound on value
    */
  case class OptimiserParam(
    name: String,
    extractor: NetworkOptimizer => Double,
    min: Double = Double.NegativeInfinity,
    max: Double = Double.PositiveInfinity,
  )

  /**
    * User interface for an optimiser
    *
    * @param name The optimiser name
    * @param params The parameters which it requires
    * @param paramConstructor A constructor which builds the optimiser from a set of parameters
    * @param default The default optimiser of this type
    * @param isInstance A type hack (type erasure sucks) for Java compatibility
    */
  case class OptimiserBuilder(
    name: String,
    params: List[OptimiserParam],
    paramConstructor: Array[Double] => NetworkOptimizer,
    default: NetworkOptimizer,
    isInstance: NetworkOptimizer => Boolean
  ) {

    // Whether the properties of this optimiser have been changed
    val dirty: Var[Boolean] = Var(false)

    // Various state trackers to check for changes and keep track of which parameters have changed
    private val initialValues: Array[Double] = params.map(_.extractor(default)).toArray
    private val paramDirty = new Array[Boolean](initialValues.size)
    private val paramValues: List[Var[Double]] = initialValues.map(Var(_)).toList

    /**
      * Reload parameters from this optimiser
      * @param opt
      */
    def selectOptimiser(opt: NetworkOptimizer): Unit = {
      if (isInstance(opt)) {
        for (i <- params.indices) {
          initialValues(i) = params(i).extractor(opt)
          paramValues(i) := initialValues(i)
          paramDirty(i) = false
        }
        dirty := false
      } else {
        selectOptimiser(default)
      }
    }

    /**
      * Build optimiser from parametrs
      * @return
      */
    def construct(): NetworkOptimizer = {
      paramConstructor(paramValues.map(_.get).toArray)
    }

    /**
      * Handler a single parameter change (param at idx changed to value)
      * @param idx
      * @param value
      */
    private def paramUpdated(idx: Int, value: Double): Unit = {
      paramDirty(idx) = value != initialValues(idx)
      dirty := paramDirty.reduce(_ || _)
    }

    @dom
    lazy val handler: Binding[html.Div] = {
      <div class="optimiser-input-container">
        {
          for (i <- Constants(params.indices :_*)) yield {
            val inputHandler = new NumericInputHandler(params(i).name, paramValues(i), params(i).min, params(i).max)
            paramUpdated(i, paramValues(i).bind)

            <div class="optimiser-input">{ inputHandler.content.bind }</div>
          }
        }
      </div>
    }
  }

  /**
    * A fully customisable SVG visualisation of a neural network
    * showing layers and connections
    *
    * @param network
    * @return
    */
  @dom
  private def networkVisualisation(network: NeuralNetwork): Binding[html.Div] = {

    /* Various properties and parameters to tweak the look of it*/

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

    /**
      * The number of neurons that will be drawn for this layer
      * @param layer
      * @return
      */
    def layerSize(layer: Int): Int = Math.min(maxNodes, network.layerSizes(layer))

    /**
      * Given a node in a particular layer, compute the (x, y) coordinates
      * of it's centre
      *
      * @param layer
      * @param idx
      * @return
      */
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
              data:stroke-width="2" data:stroke="rgba(0,0,0,0.5)"
              />
          }
        }

        <!-- Network Labels -->
        {
          for (layer <- Constants(0 until network.numLayers :_*)) yield {

            val x = layer * layerWidth + layerWidth / 2

            <g data:text-anchor="middle" data:style="font-size:12px; font-weight:300;">

              <!-- Layer type -->
              <text data:x={x.toString} data:y="15" data:style="font-size:14px; font-weight:400;">
                {
                if (layer == 0) "Input Layer"
                else if (layer == network.numLayers-1) "Output Layer"
                else "Hidden Layer " + layer
                }
              </text>

              <!-- Layer size -->
              <text data:x={x.toString} data:y="40">
                { network.layerSizes(layer) + " neurons" }
              </text>

              <!--  Activation function -->
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
      </svg>
    </div>
  }

  /**
    * Definitions for each optimiser (SGD, Adam, RMSProp) for how to build
    * its parameters from the optimiser itself
    */
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

  /**
    * The view representing all the neural network training functionality
    */
  @dom
  protected final lazy val networkViewer: Binding[html.Div] = {
    val optimiserSelect = new SelectHandler("Optimiser", optimisers.map(_.name), Constant(false))

    def currentOptimiserIndex: Int = {
      optimisers.indexWhere(_.isInstance(getOptimiser()))
    }

    optimiserSelect.selectedIndex := currentOptimiserIndex

    val currentOptimiserBinding = Var(getOptimiser())

    /**
      * Either the optimiser type has changed, or the optimiser parameters have changed ( save is needed)
      */
    val dirty = Binding {
      val currentOptimiser = currentOptimiserBinding.bind
      val optIdx = optimisers.indexWhere(_.isInstance(currentOptimiser))
      val selectedOptimiser = optimisers(optimiserSelect.selectedIndex.bind)

      optIdx != optimiserSelect.selectedIndex.bind || selectedOptimiser.dirty.bind
    }

    val buttonClasses = Binding {
      "btn-floating waves-effect waves-light tooltipped teal" + (if (dirty.bind) "" else " disabled")
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

      viewChanged()
    }

    def refreshWeightText(): Unit = {
      val elem = getElem[html.Div]("weight-text")
      elem.innerHTML = ""

      for ((weights, idx) <- getNetwork().weights.zipWithIndex) {
        elem.innerHTML += s"<strong>Layer ${idx+1}</strong><br />"
        elem.innerHTML += weights.toString.replace("\n", "<br /> ")
        elem.innerHTML += "<br/>"
      }
    }

    /* Initialisation for various components */

    initScript("optimiser-btns") { () =>
      js.Dynamic.global.$(".tooltipped").tooltip()
    }

    initScript("weight-text")(refreshWeightText)

    initScript("weight-collapsible") { () =>
      js.Dynamic.global.$(".collapsible").collapsible()
    }

    undoChanges()

    <div class="content-section">

      <h5>Network Structure</h5>
      <div class="divider"></div>

      { networkVisualisation(getNetwork()).bind }

      <ul id="weight-collapsible" class="collapsible" data:data-collapsible="accordion">
        <li>
          <div class="collapsible-header center-align">
           <i class="small material-icons">insert_chart</i>
            <strong>Weights</strong>
          </div>

          <div class="collapsible-body row">
            <div class="col s12" id="weight-text">
            </div>

            <div class="col s12 center-align">
              <br />
              <br />
              <a class="btn btn-flat waves-effect red lighten-2 white-text"
                 onclick={_:Event => refreshWeightText()}>
                Refresh
              </a>
            </div>
          </div>
        </li>
      </ul>

      <h5>Optimiser Select</h5>
      <div class="divider"></div>

      <div class="row">
        <div class="col s3">
          { optimiserSelect.handler.bind }
        </div>

        <div class="col s2" id="optimiser-btns">
          <a class={buttonClasses.bind}
            onclick={_:Event => undoChanges()} data:data-tooltip="Undo Changes">
            <i class="material-icons">undo</i>
          </a>
          <a class={buttonClasses.bind + " lighten-2"}
            onclick={_:Event => updateOptimiser()} data:data-tooltip="Save Changes">
            <i class="material-icons">save</i>
          </a>
        </div>

        <div class="col s7">
          { optimisers(optimiserSelect.selectedIndex.bind).handler.bind }
        </div>
      </div>

    </div>
  }
}
