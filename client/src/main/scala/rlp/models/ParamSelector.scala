package rlp.models

import com.thoughtworks.binding.{Binding, dom}
import com.thoughtworks.binding.Binding.Vars
import org.scalajs.dom.{Event, html}
import org.scalajs.dom.html.Div
import rlp._
import upickle.Js

class ParamSelector[A](params: Array[ModelParam[A]]) {

  val paramBindings: Vars[(ModelParam[A], Boolean)] = Vars(params.map(s => (s, s.defaultEnabled)) :_ *)
  val baseID = getGUID("param-selector")

  private def getCheckBoxID(param: ModelParam[A]): String = baseID + param.name

  private def checkBoxToggled(param: ModelParam[A]): Unit = {
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
    <div class="param-selector">
       {
       for ((param, enabled) <- paramBindings) yield {
          <div class="param-checkbox">
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

}
