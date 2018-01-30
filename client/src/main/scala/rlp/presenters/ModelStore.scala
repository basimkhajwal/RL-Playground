package rlp.presenters

import upickle.default._

case class ModelStore(
  id: Long,
  timeStamp: Long,
  environmentName: String,
  agentName: String,
  modelName: String,
  gamesPlayed: Int,
  performanceStep: Int,
  performanceHistory: Seq[Double],
  buildData: String,
  agentData: String,
)

object ModelStore {

  implicit val rw: ReadWriter[ModelStore] = macroRW
}

