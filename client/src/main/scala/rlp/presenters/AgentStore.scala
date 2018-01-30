package rlp.presenters

import upickle.default._

case class AgentStore(
  id: Long,
  timeStamp: Long,
  environmentName: String,
  agentName: String,
  name: String,
  gamesPlayed: Int,
  performanceStep: Int,
  performanceHistory: Seq[Double],
  buildData: String,
  agentData: String,
)

object AgentStore {

  implicit val rw: ReadWriter[AgentStore] = macroRW
}

