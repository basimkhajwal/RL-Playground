package rlp.pages

import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.html

trait Page {

  def start(): Unit

  val name: String
  val description: String

  @dom
  lazy val content: Binding[html.Div] = {
    <div>
      Empty game content...
    </div>
  }
}
