package rlp.ui

import com.thoughtworks.binding.{Binding, dom}
import com.thoughtworks.binding.Binding.{Constants, Var}
import org.scalajs.dom.html
import rlp._

import scala.scalajs.js

class TabSelectHandler(val items: Seq[String]) {

  private val selectedIdx: Var[Int] = Var(0)
  private val id = getGUID("tab-select-handler")
  private val tabIDs = items.map(_ => getGUID("tab-select-item"))

  def selectedIndex: Binding[Int] = selectedIdx

  private def tabChanged(input: js.Dynamic): Unit = {
    val id = input.asInstanceOf[js.Array[js.Dynamic]](0).id.asInstanceOf[String]
    selectedIdx := tabIDs.indexOf(id)
  }

  @dom
  lazy val handler: Binding[html.Div] = {

    initScript(id) { () =>

      val callback: js.Function1[js.Dynamic,Unit] = { x => tabChanged(x) }
      val tabOptions = js.Dynamic.literal("onShow" -> callback)

      js.Dynamic.global.$("#" + id).tabs(tabOptions)
    }

    <div>
      <ul id={id} class="tabs tab-select">
        {
          for ((item, tabID) <- Constants(items.zip(tabIDs) :_*)) yield {
            <li class="tab">
              <a href={"#"+tabID}>{item}</a>
            </li>
          }
        }
      </ul>

      <!-- Stubs for tabs -->
      {
        for (tabID <- Constants(tabIDs :_*)) yield {
          <div id={tabID}></div>
        }
      }
    </div>
  }

}
