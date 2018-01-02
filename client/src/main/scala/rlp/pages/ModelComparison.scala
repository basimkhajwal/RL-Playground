package rlp.pages

import com.thoughtworks.binding.Binding.{BindingSeq, Vars}
import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.{Event, html, window}
import rlp.models.Model
import rlp._

import scala.scalajs.js
import scala.scalajs.js.JSConverters._

class ModelComparison[A](
  models: Vars[Model[A]],
  performanceGap: Int
) {

  @dom
  private lazy val graph: Binding[html.Div] = {
    val plotDiv = {<div id="graph-div"></div>}
    val global = js.Dynamic.global

    val layout: js.Object = js.Dynamic.literal(
      "xaxis" -> js.Dynamic.literal("title" -> "Games Played", "zeroline" -> true),
      "yaxis" -> js.Dynamic.literal("title" -> "Performance", "zeroline" -> true),
      "showlegend" -> true
    )

    val items: BindingSeq[js.Object] = for (model:Model[A] <- models) yield {
      val history = model.performanceHistory.bind
      val xStep = performanceGap * model.performanceStep.bind

      js.Dynamic.literal(
        "x" -> history.indices.map(xStep*_).toJSArray,
        "y" -> history.toJSArray,
        "type" -> "scatter",
        "name" -> model.toString
      )
    }

    global.Plotly.newPlot(plotDiv, items.bind.toJSArray, layout)

    plotDiv
  }

  private def pageResized(): Unit = {
    val graphDiv = getElem[html.Div]("graph-div")
    if (graphDiv != null) {
      js.Dynamic.global.Plotly.Plots.resize(graphDiv)
    }
  }

  @dom
  lazy val content: Binding[html.Div] = {

    window.addEventListener("resize", { _:Event => pageResized() })
    js.timers.setTimeout(100) { pageResized() }

    <div class="card" id="model-comparison">
      <div class="row">
        <span class="card-title col s12 center-align grey lighten-3">Model Comparison</span>
        <div class="col s10 offset-s1">
          { graph.bind }
        </div>
      </div>
    </div>
  }
}
