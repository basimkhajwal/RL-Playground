package rlp.ui

import com.thoughtworks.binding.Binding.Var
import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.{Event, document, html}
import rlp._

/**
  * A UI class that encapsulates a single numeric (int or double) value
  * including a user interface with validation checks
  *
  * @param name The name to give to the input
  * @param value The var to bind to for this value
  * @param min
  * @param max
  * @param constrainInt Whether to cast to int or not
  * @param validityCheck An extra validation for the number
  */
class NumericInputHandler(
  val name: String,
  val value: Var[Double],
  val min: Double,
  val max: Double,
  val constrainInt: Boolean = false,
  val validityCheck: Double => String = { _ => "" },
) {

  /**
    * Alternate constructor for an integer-based numeric input
    *
    * @param name
    * @param value
    * @param min
    * @param max
    */
  def this(name: String, value: Var[Int], min: Int, max: Int) {
    this(name, NumericInputHandler.wrapDouble(value), min, max, true)
  }

  private val inputID = getGUID("numeric-input")

  /**
    * Handle a data change and a possible validation error
    */
  private def dataChanged(): Unit = {
    val inputElem = getElem[html.Input](inputID)
    val labelElem = document.querySelector("label[for=\"" + inputID + "\"]")

    if (inputElem.validity.valid) {

      // If passes default validation, check for custom validation

      val inputValue = inputElem.value.toDouble
      val error = validityCheck(inputValue)

      inputElem.setCustomValidity(error)
      labelElem.setAttribute("data-error", error)

      if (error == "") {
        value := inputValue
      }

    } else {

      // Default validation error
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

  /**
    * Clever use of double-binding to wrap
    * an int-var as a double var whilst
    * maintaining type safety
    *
    * @param intVar
    * @return
    */
  def wrapDouble(intVar: Var[Int]): Var[Double] = {
    val doubleVar = Var[Double](intVar.get)

    // Forward binding
    Binding {
      val intVal = intVar.bind
      doubleVar := intVal
    } watch()

    // Backward binding
    Binding {
      val doubleVal = doubleVar.bind
      intVar := doubleVal.toInt
    } watch()

    doubleVar
  }
}