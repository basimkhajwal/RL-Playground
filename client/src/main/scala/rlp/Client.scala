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
  lazy val header: Binding[html.Div] = {
    <div class="navbar-fixed">
      <nav class="teal darken-2">
        <div class="nav-wrapper page-container">
          <a href="#" class="white-text brand-logo">RL-Playground</a>
          <ul class="right">
            <li><a href="#">Home</a></li>
            <li><a href="#">Leaderboard</a></li>
            <li><a class="btn waves-effect waves-light red">Login</a></li>
            <li><a class="btn waves-effect waves-light blue">Sign Up</a></li>
          </ul>
        </div>
      </nav>
    </div>
  }

  @dom
  lazy val footer: Binding[html.Element] = {
    <footer class="page-footer deep-orange lighten-2">
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
  }

  @dom
  lazy val home: Binding[html.Div] = {

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

      { header.bind }

      <div class="row page-container"> { pageSelect.handler.bind } </div>

      { pages(pageSelect.selectedIndex.bind).content.bind }

      { footer.bind }

      {
        pageChanged(pageSelect.selectedIndex.bind)
        ""
      }
    </div>
  }

  def main(args: Array[String]): Unit = {
    dom.render(document.getElementById("clientContainer"), home)
    SelectHandler.init()
  }
}
