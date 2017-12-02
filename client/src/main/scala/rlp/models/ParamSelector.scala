package rlp.models

import com.thoughtworks.binding.{Binding, dom}
import com.thoughtworks.binding.Binding.Vars
import org.scalajs.dom.{Event, html}
import org.scalajs.dom.html.Div
import rlp._

class ParamSelector[A](params: Array[ModelParam[A]]) {

  val paramsEnabled: Vars[(ModelParam[A], Boolean)] = Vars(params.map(s => (s, s.defaultEnabled)) :_ *)
  val baseID = getGUID("param-selector")

  private def getCheckBoxID(param: ModelParam[A]): String = baseID + param.name

  private def checkBoxToggled(param: ModelParam[A]): Unit = {
    val idx = params.indexOf(param)
    val checkBox = getElem[html.Input](getCheckBoxID(param))

    paramsEnabled.get(idx) = (param, checkBox.checked)
  }

  @dom
  lazy val builder: Binding[Div] = {
    <div class="param-selector">
       {
       for ((param, enabled) <- paramsEnabled) yield {
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
