package rlp.pages

import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.html

/**
  * Page interface
  */
trait Page {

  // The name to display (to select the page)
  val name: String

  /**
    * Show the page to the user in the foreground
    */
  def show(): Unit

  /**
    * Move the page to the background
    */
  def hide(): Unit

  /**
    * The HTML content of the page itself
    */
  @dom
  lazy val content: Binding[html.Div] = {
    <div>
      Empty game content...
    </div>
  }
}
