package rlp.utils

import com.thoughtworks.binding.Binding.Var
import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.{Event, html, document}
import rlp._

class NumericInputHandler(
  val name: String,
  val value: Var[Double],
  val min: Double,
  val max: Double,
  val constrainInt: Boolean = false,
  val validityCheck: Double => String = { _ => "" },
) {

  def this(name: String, value: Var[Int], min: Int, max: Int) {
    this(name, NumericInputHandler.wrapDouble(value), min, max, true)
  }

  private val inputID = getGUID("numeric-input")

  private def dataChanged(): Unit = {
    val inputElem = getElem[html.Input](inputID)
    val labelElem = document.querySelector("label[for=\"" + inputID + "\"]")

    if (inputElem.validity.valid) {

      val inputValue = inputElem.value.toDouble
      val error = validityCheck(inputValue)

      inputElem.setCustomValidity(error)
      labelElem.setAttribute("data-error", error)

      if (error == "") {
        value := inputValue
      }

    } else {
      labelElem.setAttribute("data-error", s"$name must be between $min and $max")
    }
  }

  @dom
  lazy val content: Binding[html.Div] = {

    <div class="input-field">
      <input id={inputID} class="validate" type="number" step={if (constrainInt) "1" else "any"}
             min={min.toString} max={max.toString} oninput={_:Event => dataChanged() }
             value={value.bind.toString} required={true} />
      <label class="active" for={inputID}>{name}</label>
    </div>
  }
}

object NumericInputHandler {
  def wrapDouble(intVar: Var[Int]): Var[Double] = {
    val doubleVar = Var[Double](intVar.get)

    Binding {
      val intVal = intVar.bind
      doubleVar := intVal
    } watch()

    Binding {
      val doubleVal = doubleVar.bind
      intVar := doubleVal.toInt
    } watch()

    doubleVar
  }
}