package rlp.controllers

import rlp.ai.agents.QTableAgent
import rlp.environment.{Agent, Pong}

class QTableController extends ModelController[Pong.State, Pong.AgentState, Pong.Action] {

  import Pong._

  override val name: String = "Tabular Q Learning"



  override def getTrainedAgent(): Agent[AgentState, Action] = ???

  override def trainStep(): Unit = ???

  override def getTrainingState() = ???
}
