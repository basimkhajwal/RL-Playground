package rlp.controllers

import rlp.environment.{Agent, Pong}

class PongQNetworkController extends ModelController[Pong.State, Pong.AgentState, Pong.Action] {

  import Pong._

  override val name: String = "Neural Network Q Learning"

  override def getTrainedAgent(): Agent[AgentState, Action] = ???

  override def trainStep(): Unit = ???

  override def getTrainingState() = ???
}
