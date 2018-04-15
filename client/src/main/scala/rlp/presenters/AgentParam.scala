package rlp.presenters

/**
  * Represents a single build-time parameter to an agent presenter
  *
  * @param name
  * @param value
  * @param defaultEnabled
  * @tparam T
  */
case class AgentParam[T](
  name: String,
  value: T,
  defaultEnabled: Boolean = true
)
