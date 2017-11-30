package rlp.models

import com.thoughtworks.binding.Binding.{Var, Vars}
import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.{Event, html}
import org.scalajs.dom.html.Div
import org.scalajs.dom.raw.HTMLElement
import rlp.agent.QNetworkAgent.QNetworkSpace
import rlp.agent.{Agent, QNetworkAgent}
import rlp.ai.ActivationFunction.{Linear, ReLU, Sigmoid}
import rlp.ai.{ActivationFunction, NeuralNetwork}
import rlp._

class QNetworkModel[S,A](
  numActions: Int,
  actionMap: Int => A,
  spaces: Array[QNetworkSpace[S]]
) extends Model[Agent[S,A]](QNetworkModel.name){

  // TODO: Implement these properly
  override def cloneBuild(): Model[Agent[S, A]] = this
  override protected def _duplicate(): Model[Agent[S, A]] = this

  private val numStates = spaces.map(s => s.size).sum

  private var qNetwork: QNetworkAgent = _

  case class LayerDef(
    name: String,
    size: Var[Int] = Var(10),
    activation: ActivationFunction = ReLU,
    fixed: Boolean = false
  )

  private val layerDefinitions = Vars[LayerDef](LayerDef("Input Layer", Var(numStates), fixed = true) )

  @dom
  def layerDefinition(layer: LayerDef): Binding[Div] = {

    val inputID = "input-size-" + layer.name

    def inputChanged(): Unit = {
      val inputElem = getElem[html.Input](inputID)
      layer.size := inputElem.value.toInt
    }

    <div class="layer-definition row">
      <span class="col s3">{layer.name}</span>
      <div class="input-field col s4">
        <input id={inputID} disabled={layer.fixed}
          value={layer.size.bind.toString} onchange={_:Event => inputChanged()}
          class="validate" type="number" min="1" max="100" step="1" />
        <label for={inputID} data:data-error="Enter size in range 1-100">Layer Size</label>
      </div>
      <div class="input-field col s4">
        <!-- TODO: Do activation function select -->
      </div>
      <div class="col s1">
        <!-- TODO: Do delete button if not fixed -->
      </div>
    </div>
  }

  @dom
  override lazy val modelBuilder: Binding[HTMLElement] = {
    <div class="row">
      <h5 class="col s11 offset-s1 light">Layer Definitions</h5>
      <div id="layer-definitions" class="col s10 offset-s1">
      </div>
    </div>
  }

  override def buildAgent(): Agent[S, A] = {

    val network = new NeuralNetwork(Array(numStates, 10, numActions), Array(Sigmoid, Sigmoid))
    network.randomiseWeights(-0.5, 0.5)

    qNetwork = new QNetworkAgent(network)

    QNetworkAgent.build(qNetwork, actionMap, spaces)
  }

  override def resetAgent(): Unit = {
    qNetwork.reset()
  }
}

object QNetworkModel {

  val name = "Q Network"

  def builder[S,A](numActions: Int, actionMap: Int => A, spaces: QNetworkSpace[S]*): Model.Builder[Agent[S,A]] = {
    name -> (() => new QNetworkModel(numActions, actionMap, spaces.toArray))
  }
}
