package rlp

import com.thoughtworks.binding.Binding.Var
import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.{Event, document, html, window}
import rlp.pages._
import rlp.ui.{SelectHandler, TabSelectHandler}

import scala.scalajs.js.Dynamic

/**
  * The main entry point to the application which arbitrates the page select
  * and which page currently has focus
  */
object Client {

  // The pages available to select from
  val pages: List[Page] = List(
    new FlappyBirdPage(),
    new MountainCarPage(),
    new PongPage(),
    new PuckWorldPage()
  )

  /**
    * The app itself consisting of a tab-select and the current page
    */
  @dom
  lazy val app: Binding[html.Div] = {

    val pageSelect = new TabSelectHandler(pages.map(_.name))
    val currentPage = Var[Page](null)

    /**
      * When a page is changed, hide the previous one and show the new one
      *
      * @param idx
      */
    def pageChanged(idx: Int): Unit = {
      if (currentPage.get != null) {
        currentPage.get.hide()
        currentPage := pages(idx)
        currentPage.get.show()
      } else {
        window.onload = { _:Event =>
          currentPage := pages(idx)
          currentPage.get.show()
        }
      }

      Dynamic.global.$("select").material_select()
    }

    <div id="app" class="grey lighten-5">

      <div class="row page-container">
        { pageSelect.handler.bind }
      </div>

      {
        val page = currentPage.bind
        if (page != null) {
          page.content.bind
        } else {
          <!-- -->
        }
      }

      {
        pageChanged(pageSelect.selectedIndex.bind)
        ""
      }
    </div>
  }

  // The app entry point
  def main(args: Array[String]): Unit = {

    // Render the app onto the clientContainer element
    dom.render(document.getElementById("clientContainer"), app)

    // Update the select elements
    SelectHandler.init()

    // Hide the pre-loader
    getElem[html.Div]("loader").classList.remove("active")
  }
}
