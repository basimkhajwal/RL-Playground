package rlp.ui

import com.thoughtworks.binding.Binding.{BindingSeq, Constant, Constants, Var}
import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.html.Div
import org.scalajs.dom.raw.AnimationEvent
import org.scalajs.dom.{Event, html, window}
import rlp._

import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js.{Dynamic, timers}

object SelectHandler {

  /**
    * This system handles inserting select handlers dynamically which causes
    * an issue with the materialize initializer.
    *
    * To overcome this, the handler listens for the CSS animation (that automatically
    * starts on insert) so that it can notify materialize.
    */

  private val handlers: ArrayBuffer[SelectHandler] = ArrayBuffer.empty

  def init(): Unit = {
    window.addEventListener(
      "animationstart",
      { e:AnimationEvent => if (e.animationName == "selectInserted") onChange() }
      , false
    )
  }

  private def onChange(): Unit = {
    for (h <- handlers) h.refresh()
  }

  def register(handler: SelectHandler): Unit = {
    handlers.append(handler)
  }
}

/**
  * UI component that encapsulates a select HTML5 component with the materialize library
  *
  * @param name The title of the select box
  * @param items The items to show
  * @param disabledCondition Whether to disable the select or not
  */
class SelectHandler(val name: String, val items: BindingSeq[String], val disabledCondition: Binding[Boolean]) {

  SelectHandler.register(this)

  def this(name: String, items: BindingSeq[String]) = {
    this(name, items, Constant(false))
  }

  def this(name: String, items: Seq[String]) = {
    this(name, Constants(items:_*))
  }

  def this(name: String, items: Seq[String], disabledCondition: Binding[Boolean]) = {
    this(name, Constants(items:_*), disabledCondition)
  }

  val selectID = getGUID("select-handler")
  val selectedIndex = Var(0)

  private def selectionChanged(): Unit = {
    val select = getElem[html.Select](selectID)
    selectedIndex := select.selectedIndex
  }

  /**
    * Update the items in the container
    */
  private def refresh(): Unit = {
    val select = getElem[html.Select](selectID)
    if (select != null && select.selectedIndex != selectedIndex.get) {
      select.selectedIndex = selectedIndex.get
    }
  }

  /**
    * Fixes issue with materialize library not handling updated elements
    *
    * @param condition
    * @param innerItems
    */
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
      <select id={selectID} class="browser-default"
        onchange={_:Event => selectionChanged()} disabled={disabledCondition.bind}
        value={selectedIndex.toString}>
        {
          val indexedItems = Constants(items.bind.zipWithIndex: _*)
          for ((item, idx) <- indexedItems) yield {
            <option value={idx.toString}
                selected={idx == selectedIndex.bind}
            >{item}</option>
          }
        }
      </select>

      {
        contentChanged(disabledCondition.bind, items.bind)
        ""
      }
    </div>
  }
}
