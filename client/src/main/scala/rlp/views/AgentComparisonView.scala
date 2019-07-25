package rlp.views

import com.thoughtworks.binding.Binding.{BindingSeq, Vars}
import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.{Event, html, window}
import rlp._
import rlp.presenters.AgentPresenter

import scala.scalajs.js
import scala.scalajs.js.JSConverters._

/**
  * View to show a graph comparing the performance
  * over time of all the agents
  *
  * @param agents A binding to all the agents
  * @param performanceGap The gap (time interval) between each performance entry
  * @tparam A The agent type
  */
class AgentComparisonView[A](
  agents: Vars[AgentPresenter[A]],
  performanceGap: Int
) {

  /**
    * A binding to the Plotly graph object
    */
  @dom
  private lazy val graph: Binding[html.Div] = {

    // The <div> which wraps the graph
    val plotDiv = {<div id="graph-div"></div>}

    // Generate a list of data-points (JS objects) for each agent
    val items: BindingSeq[js.Object] =
      for (agent:AgentPresenter[A] <- agents) yield {

        val history = agent.performanceHistory.bind
        val xStep = performanceGap * agent.performanceStep.bind

        js.Dynamic.literal(
          "x" -> history.indices.map(xStep*_).toJSArray,
          "y" -> history.toJSArray,
          "type" -> "scatter",
          "name" -> agent.toString
        )
      }

    // Layout preferences for the graph
    val layout: js.Object = js.Dynamic.literal(
      "xaxis" -> js.Dynamic.literal("title" -> "Games Played", "zeroline" -> true, "y" -> 0.05),
      "yaxis" -> js.Dynamic.literal("title" -> "Performance", "zeroline" -> true),
      "showlegend" -> true,
      "legend" -> js.Dynamic.literal("orientation" -> "h"),
      "title" -> js.Dynamic.literal("text" -> "Agent Comparison")
    )

    // Options
    val options: js.Object = js.Dynamic.literal(
      "displayModeBar" -> false
    )

    // Generate the graph itself through plotly.js
    js.Dynamic.global.Plotly.newPlot(plotDiv, items.bind.toJSArray, layout, options)

    plotDiv
  }

  // Re-draw the graph on page resize
  private def pageResized(): Unit = {
    val graphDiv = getElem[html.Div]("graph-div")
    if (graphDiv != null) {
      js.Dynamic.global.Plotly.Plots.resize(graphDiv)
    }
  }

  @dom
  lazy val content: Binding[html.Div] = {

    window.addEventListener(
      "resize",
      { _:Event => pageResized() }
    )

    js.timers.setTimeout(100) {
      pageResized()
    }

    <div id="agent-comparison">
      { graph.bind }
    </div>
  }
}
