package rlp.presenters

import upickle.default._

/**
  * Store the entire state of a single agent entity, used as an intermediate
  * value between the AgentPresenters and the IndexedDB handlers
  *
  * @param id
  * @param timeStamp
  * @param environmentName
  * @param agentName
  * @param name
  * @param gamesPlayed
  * @param performanceStep
  * @param performanceHistory
  * @param buildData
  * @param agentData
  */
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

  /**
    * Generate an implicit read/writer for the functions, from the case-class definition
    * AgentStore -> JSON (write)
    * JSON -> AgentStore (read)
    */
  implicit val rw: ReadWriter[AgentStore] = macroRW
}

