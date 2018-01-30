package rlp.presenters

import com.thoughtworks.binding.{Binding, dom}
import com.thoughtworks.binding.Binding.Vars
import org.scalajs.dom.{Event, html}
import org.scalajs.dom.html.Div
import rlp._
import upickle.Js

class ParamSelector[A](params: Array[AgentParam[A]]) {

  val paramBindings: Vars[(AgentParam[A], Boolean)] = Vars(params.map(s => (s, s.defaultEnabled)) :_ *)
  val baseID = getGUID("param-selector")

  private def getCheckBoxID(param: AgentParam[A]): String = baseID + param.name

  private def checkBoxToggled(param: AgentParam[A]): Unit = {
    val idx = params.indexOf(param)
    val checkBox = getElem[html.Input](getCheckBoxID(param))

    paramBindings.get(idx) = (param, checkBox.checked)
  }

  def store(): Js.Value = {
    Js.Arr(
      paramBindings.get.map(p =>
        Js.Obj(
          "name" -> Js.Str(p._1.name),
          "enabled" -> (if (p._2) Js.True else Js.False)
        )
      )
      :_*
    )
  }

  def load(data: Js.Value): Unit = {
    val paramMap = data.arr.map { p =>
      val paramKey = p.obj
      (paramKey("name").str, paramKey("enabled") == Js.True)
    }.toMap

    val newParams = paramBindings.get.map(p => (p._1, paramMap(p._1.name)))
    paramBindings.get.clear()
    paramBindings.get.appendAll(newParams)
  }

  @dom
  lazy val builder: Binding[Div] = {
    <div class="distribute-container">
       {
         for ((param, enabled) <- paramBindings) yield {
            <div class="distribute-item">
               <input type="checkbox" id={getCheckBoxID(param)}
                      onchange={ _:Event => checkBoxToggled(param)}
                      checked={enabled}
               />
               <label for={getCheckBoxID(param)}>{param.name}</label>
            </div>
         }
       }
    </div>
  }

  @dom
  lazy val viewer: Binding[Div] = {
    <div class="content-section">
      <h5>Inputs Enabled</h5>
      <div class="divider"></div>

      <div class="distribute-container">
        {
          for ((param, enabled) <- paramBindings; if enabled) yield {
            <div class="distribute-item">
              <h6>{param.name}</h6>
            </div>
          }
        }
      </div>
    </div>
  }
}
