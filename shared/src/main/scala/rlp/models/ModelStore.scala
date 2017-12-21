package rlp.models

case class ModelStore(
  environmentName: String,
  agentName: String,
  modelName: String,
  gamesPlayed: Int,
  performanceHistory: Seq[Double],
  buildData: String,
  agentData: String
)

