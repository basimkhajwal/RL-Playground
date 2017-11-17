package rlp

import com.thoughtworks.binding.dom
import org.scalajs.dom.document
import rlp.pages.PongPage

import scala.scalajs.js
import scala.scalajs.js.Dynamic


object Client {

  def main(args: Array[String]): Unit = {
    val page = new PongPage()
    dom.render(document.getElementById("clientContainer"), page.content)

    Dynamic.global.$("select").material_select()

    page.start()
  }
}
