package rlp.views

import com.thoughtworks.binding.Binding.{BindingSeq, Vars}
import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.{Event, html, window}
import rlp._
import rlp.presenters.AgentPresenter

import scala.scalajs.js
import scala.scalajs.js.JSConverters._

class AgentComparisonView[A](
  agents: Vars[AgentPresenter[A]],
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

    val items: BindingSeq[js.Object] = for (agent:AgentPresenter[A] <- agents) yield {
      val history = agent.performanceHistory.bind
      val xStep = performanceGap * agent.performanceStep.bind

      js.Dynamic.literal(
        "x" -> history.indices.map(xStep*_).toJSArray,
        "y" -> history.toJSArray,
        "type" -> "scatter",
        "name" -> agent.toString
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

    <div class="card" id="agent-comparison">
      <div class="row">
        <span class="card-title col s12 center-align grey lighten-3">Agent Comparison</span>
        <div class="col s10 offset-s1">
          { graph.bind }
        </div>
      </div>
    </div>
  }
}
