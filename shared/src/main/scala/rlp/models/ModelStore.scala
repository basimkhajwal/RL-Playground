package rlp.models

import upickle.default._

case class ModelStore(
  environmentName: String,
  agentName: String,
  modelName: String,
  gamesPlayed: Int,
  performanceHistory: Seq[Double],
  buildData: String,
  agentData: String
)

object ModelStore {

  implicit val rw: ReadWriter[ModelStore] = macroRW
}

