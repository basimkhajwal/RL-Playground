package rlp

import com.thoughtworks.binding.Binding.Constant
import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.{Event, document, html, window}
import rlp.pages.{FlappyBirdPage, Page, PongPage}
import rlp.utils.SelectHandler

import scala.scalajs.js.Dynamic

object Client {

  val pages: List[Page] = List(
    new PongPage(),
    new FlappyBirdPage()
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

    <div id="app" class="grey lighten-5">
      <div class="navbar-fixed">
        <nav class="teal">
          <div class="nav-wrapper page-container">
            <div class="row">
              <div class="col s3">
                <a href="#" class="brand-logo">RL-Playground</a>
              </div>
              <p id="subtitle" class="col s4 offset-s1">An interactive reinforcement learning demonstration</p>
            </div>
          </div>
        </nav>
      </div>

      <div class="row page-container"> { pageSelect.handler.bind } </div>

      { pages(pageSelect.selectedIndex.bind).content.bind }

      <footer class="page-footer">
        <div class="page-container">
          <div class="row">
            <div class="col l6 s12">
              <h5 class="white-text">RL-Playground</h5>
              <p class="grey-text text-lighten-4">
                A reinforcement learning playground made for my A-level computing coursework.
                Made using Scala.JS and Binding.scala.
              </p>
            </div>
            <div class="col l4 offset-l2 s12">
              <h5 class="white-text">Links</h5>
              <ul>
                <li><a class="grey-text text-lighten-3" href="#">Home</a></li>
                <li><a class="grey-text text-lighten-3" href="#">Leaderboard</a></li>
                <li>
                  <iframe
                    src={"https://ghbtns.com/github-btn.html?user=basimkhajwal&repo=RL-Playground&type=star&count=true"}
                    data:frameborder="0" data:scrolling="0" width="160px" height="30px"></iframe>
                </li>
              </ul>
            </div>
          </div>
        </div>
        <div class="footer-copyright">
          <div class="page-container">© 2017 Basim Khajwal</div>
        </div>
      </footer>

      {
        pageChanged(pageSelect.selectedIndex.bind)
        ""
      }
    </div>
  }

  def main(args: Array[String]): Unit = {
    dom.render(document.getElementById("clientContainer"), app)

    SelectHandler.init()
  }
}
