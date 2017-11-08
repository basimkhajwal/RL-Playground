package rlp

import com.thoughtworks.binding.dom
import org.scalajs.dom.document
import rlp.pages.PongPage


object Client {

  def main(args: Array[String]): Unit = {
    val page = new PongPage()
    dom.render(document.getElementById("clientContainer"), page.content)
  }
}
