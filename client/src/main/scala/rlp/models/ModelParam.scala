package rlp.models

case class ModelParam[T](
  name: String,
  value: T,
  defaultEnabled: Boolean = true
)
