package rlp.pages

import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.html

trait Page {

  val name: String

  def show(): Unit

  def hide(): Unit

  @dom
  lazy val content: Binding[html.Div] = {
    <div>
      Empty game content...
    </div>
  }
}
