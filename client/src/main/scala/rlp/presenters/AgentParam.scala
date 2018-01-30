package rlp.presenters

case class AgentParam[T](
  name: String,
  value: T,
  defaultEnabled: Boolean = true
)
