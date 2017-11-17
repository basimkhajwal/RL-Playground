package rlp

import com.thoughtworks.binding.Binding.{Constants, Var}
import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.Event
import org.scalajs.dom.document
import org.scalajs.dom.html.{Div, Select}

import scala.scalajs.js.Dynamic

object SelectHandler {

  private var counter = 0

  def generateUID(): String = {
    counter += 1
    "select--handler--" + counter
  }
}

class SelectHandler(val name: String, val items: List[String], val disabledCondition: Binding[Boolean]) {

  val selectID = SelectHandler.generateUID()
  val selectedIndex = Var(0)

  private def selectionChanged(): Unit = {
    val select = document.getElementById(selectID).asInstanceOf[Select]
    selectedIndex := select.selectedIndex
  }

  private def conditionToggled(condition: Boolean): Unit = {
    Dynamic.global.$("#" + selectID).material_select()
  }

  @dom
  lazy val handler: Binding[Div] = {
    <div class="input-field">
      <select id={selectID} onchange={_:Event => selectionChanged()} disabled={disabledCondition.bind}>
        {
          for ((item, idx) <- Constants(items.zipWithIndex: _*)) yield {
            <option value={idx.toString}
                selected={idx == selectedIndex.bind}
            >{item}</option>
          }
        }
      </select>
      <label>{name}</label>

      { conditionToggled(disabledCondition.bind); "" }
    </div>
  }
}
