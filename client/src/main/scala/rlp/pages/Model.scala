package rlp.pages

import rlp.controllers.ModelController

case class Model[A](
  name: String,
  controller: ModelController[A],
  id: Long = System.currentTimeMillis()
) {
}
