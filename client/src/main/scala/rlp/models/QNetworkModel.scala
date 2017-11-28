package rlp.models

import rlp.agent.QNetworkAgent.QNetworkSpace
import rlp.agent.{Agent, QNetworkAgent}
import rlp.ai.ActivationFunction.Sigmoid
import rlp.ai.NeuralNetwork

class QNetworkModel[S,A](
  numActions: Int,
  actionMap: Int => A,
  spaces: Array[QNetworkSpace[S]]
) extends Model[Agent[S,A]](QNetworkModel.name){

  // TODO: Implement these properly
  override def cloneBuild(): Model[Agent[S, A]] = this
  override protected def _duplicate(): Model[Agent[S, A]] = this

  private var qNetwork: QNetworkAgent = _

  override def buildAgent(): Agent[S, A] = {

    val numStates = spaces.map(s => s.size).sum

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
