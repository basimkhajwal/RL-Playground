package rlp

import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.{Event, document, html, window}
import rlp.pages.{FlappyBirdPage, Page, PongPage}
import rlp.ui.{SelectHandler, TabSelectHandler}

import scala.scalajs.js.Dynamic

object Client {

  val pages: List[Page] = List(
    new PongPage(),
    new FlappyBirdPage()
  )

  @dom
  lazy val app: Binding[html.Div] = {

    val pageSelect = new TabSelectHandler(pages.map(_.name))
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

    <div id="app" class="grey lighten-5">
      <div class="row page-container"> { pageSelect.handler.bind } </div>

      { pages(pageSelect.selectedIndex.bind).content.bind }

      {
        pageChanged(pageSelect.selectedIndex.bind)
        ""
      }
    </div>
  }

  def main(args: Array[String]): Unit = {
    dom.render(document.getElementById("clientContainer"), app)
    SelectHandler.init()

    /* Hide pre-loader */
    getElem[html.Div]("loader").classList.remove("active")
  }
}
