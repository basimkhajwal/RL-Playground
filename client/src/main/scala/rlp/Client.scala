package rlp

import com.thoughtworks.binding.Binding.Constant
import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.{Event, document, html, window}
import rlp.pages.{Page, PongPage}
import rlp.utils.SelectHandler

import scala.scalajs.js.Dynamic

object Client {

  val pages: List[Page] = List(
    new PongPage()
  )

  @dom
  lazy val app: Binding[html.Div] = {

    val pageSelect = new SelectHandler("Select Game", pages.map(_.name), Constant(false))
    var prevPage: Page = null

    def pageChanged(idx: Int): Unit = {
      if (prevPage != null) {
        prevPage.stop()
        prevPage = pages(idx)
        prevPage.start()
      } else {
        window.onload = { _:Event =>
          prevPage = pages(idx)
          prevPage.start()
        }
      }

      Dynamic.global.$("select").material_select()
    }

    <div id="app">
      <nav class="teal z-depth-0">
        <div class="nav-wrapper page-container">
          <a href="#" class="brand-logo left">RL-Playground</a>
          <p id="subtitle" class="right hide-on-med-and-down">
            An interactive reinforcement learning demonstration
          </p>
        </div>
      </nav>

      <div class="row page-container">
        { pageSelect.handler.bind }
      </div>

      { pages(pageSelect.selectedIndex.bind).content.bind }

      {
        pageChanged(pageSelect.selectedIndex.bind)
        ""
      }
    </div>
  }

  def main(args: Array[String]): Unit = {
    dom.render(document.getElementById("clientContainer"), app)
  }
}
