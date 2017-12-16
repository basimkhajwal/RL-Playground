package rlp

import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.{document, html}
import rlp.pages.{GamePage, Page, PongPage}

import scala.scalajs.js
import scala.scalajs.js.Dynamic


object Client {

  val page: Page = new PongPage()

  @dom
  lazy val app: Binding[html.Div] = {
    <div id="app">
      <nav class="teal z-depth-0">
        <div class="nav-wrapper page-container">
          <a href="#" class="brand-logo left">RL-Playground</a>
          <p id="subtitle" class="right hide-on-med-and-down">
            An interactive reinforcement learning demonstration
          </p>
        </div>
      </nav>

      { page.content.bind }
    </div>
  }

  def main(args: Array[String]): Unit = {
    dom.render(document.getElementById("clientContainer"), app)

    Dynamic.global.$("select").material_select()

    page.start()
  }
}
