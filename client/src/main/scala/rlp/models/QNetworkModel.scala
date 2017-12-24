package rlp.models

import rlp.agent.QNetworkAgent.QNetworkSpace
import rlp.agent.{Agent, QNetworkAgent}
import rlp.ai.NeuralNetwork
import rlp._
import rlp.ai.optimizers.NetworkOptimizer
import upickle.Js

class QNetworkModel[S,A](
  environment: String,
  numActions: Int,
  actionMap: Int => A,
  params: Array[ModelParam[QNetworkSpace[S]]]
) extends NetworkModel[S, A, QNetworkSpace[S]](
  environment, QNetworkModel.name,
  params,
  { p => p.size }, numActions
) {

  private var qNetwork: QNetworkAgent = _

  override lazy val modelBuilder = networkBuilder

  override def buildAgent(): Agent[S, A] = {
    val inputs = for ((param, enabled) <- paramBindings.get; if enabled) yield param.value
    val network = buildNetwork()

    qNetwork = new QNetworkAgent(network)
    qNetwork.reset()

    QNetworkAgent.build(qNetwork, actionMap, inputs)
  }

  override def cloneBuildFrom(that: Model[Agent[S,A]]): Unit = {
    super.cloneBuildFrom(that)

    val controller = that.asInstanceOf[QNetworkModel[S,A]]
    paramBindings.get.clear()
    paramBindings.get ++= controller.paramBindings.get
  }

  override lazy val modelViewer = networkViewer

  override def resetAgent(): Unit = {
    super.resetAgent()
    qNetwork.reset()
  }

  override protected def getNetwork(): NeuralNetwork = qNetwork.network

  override protected def getOptimiser(): NetworkOptimizer = qNetwork.optimiser

  override protected def setOptimiser(optimiser: NetworkOptimizer): Unit = {
    qNetwork.optimiser = optimiser
  }

  override protected def storeBuild(): Js.Value = Js.Obj()

  override protected def storeAgent(): Js.Value = qNetwork.store()

  override protected def loadBuild(build: Js.Value): Unit = ???

  override protected def loadAgent(build: Js.Value): Unit = ???
}

object QNetworkModel {

  val name = "Q Network"

  def builder[S,A](
    environment: String,
    numActions: Int, actionMap: Int => A,
    params: ModelParam[QNetworkSpace[S]]*): Model.Builder[Agent[S,A]] = {

    name -> (() => new QNetworkModel(environment, numActions, actionMap, params.toArray))
  }
}
