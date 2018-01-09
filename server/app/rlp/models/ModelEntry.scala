package rlp.models

case class ModelEntry(
  id: Long,
  userId: Long,
  environmentName: String,
  agentName: String,
  gamesPlayed: Long,
)

