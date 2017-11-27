package rlp.pages

import com.thoughtworks.binding.Binding.Var
import rlp.controllers.ModelController
import org.scalajs.dom.window.performance

case class Model[A](
  name: String,
  controller: ModelController[A],
  gamesPlayed: Var[Int] = Var(0),
  id: Long = (1000 * performance.now()).toLong
) {

  def duplicate(): Model[A] = {
    Model(name + " copy", controller.duplicate(), Var(gamesPlayed.get))
  }

  override def toString: String = controller.name + " - " + name
}
