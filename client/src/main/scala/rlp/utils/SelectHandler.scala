package rlp.utils

import com.thoughtworks.binding.Binding.{BindingSeq, Constants, Var}
import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.{Event, html}
import org.scalajs.dom.html.{Div}

import scala.scalajs.js.timers
import rlp._

import scala.scalajs.js.Dynamic

class SelectHandler(val name: String, val items: BindingSeq[String], val disabledCondition: Binding[Boolean]) {

  def this(name: String, items: Seq[String], disabledCondition: Binding[Boolean]) = {
    this(name, Constants(items:_*), disabledCondition)
  }

  val selectID = getGUID("select-handler")
  val selectedIndex = Var(0)

  private def selectionChanged(): Unit = {
    val select = getElem[html.Select](selectID)
    selectedIndex := select.selectedIndex
  }

  private def contentChanged(condition: Boolean, innerItems: Seq[String]): Unit = {
    Dynamic.global.$("#" + selectID).material_select()
    timers.setTimeout(20) {
      Dynamic.global.$("#" + selectID).material_select()
    }
  }

  @dom
  lazy val handler: Binding[Div] = {

    <div>
      <label>{name}</label>
      <select id={selectID} class="browser-default" onchange={_:Event => selectionChanged()} disabled={disabledCondition.bind}>
        {
          val indexedItems = Constants(items.bind.zipWithIndex: _*)
          for ((item, idx) <- indexedItems) yield {
            <option value={idx.toString}
                selected={idx == selectedIndex.bind}
            >{item}</option>
          }
        }
      </select>

      { contentChanged(disabledCondition.bind, items.bind); "" }
    </div>
  }
}
