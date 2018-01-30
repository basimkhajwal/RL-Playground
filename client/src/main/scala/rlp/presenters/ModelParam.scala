package rlp.presenters

case class ModelParam[T](
  name: String,
  value: T,
  defaultEnabled: Boolean = true
)
