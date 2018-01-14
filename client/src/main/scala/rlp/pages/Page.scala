package rlp.pages

import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.html
import rlp.dao.ModelDAO

trait Page {

  protected var modelDAO: ModelDAO = _

  def setModelDAO(modelDAO: ModelDAO): Unit = {
    this.modelDAO = modelDAO
  }

  def start(): Unit

  def stop(): Unit

  val name: String

  @dom
  lazy val content: Binding[html.Div] = {
    <div>
      Empty game content...
    </div>
  }
}
