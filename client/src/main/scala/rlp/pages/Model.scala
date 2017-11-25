package rlp.pages

import com.thoughtworks.binding.Binding.Var
import rlp.controllers.ModelController

case class Model[A](
  name: String,
  controller: ModelController[A],
  gamesPlayed: Var[Int] = Var(0),
  id: Long = System.currentTimeMillis()
) {
}
