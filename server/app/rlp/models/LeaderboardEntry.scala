package rlp.models

case class LeaderboardEntry(
  id: Long,
  userId: Long,
  timeStamp: Long,
  environmentName: String,
  agentName: String,
  modelName: String,
  gamesPlayed: Int,
  score: Double
)
