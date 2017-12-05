package rlp.pages

import com.definitelyscala.plotlyjs._
import com.definitelyscala.plotlyjs.all._
import com.definitelyscala.plotlyjs.PlotlyImplicits._
import com.definitelyscala.plotlyjs.plotlyConts._
import com.thoughtworks.binding.Binding.{BindingSeq, Vars}
import com.thoughtworks.binding.{Binding, dom}
import org.querki.jsext.JSOptionBuilder
import org.scalajs.dom.{Event, html, window}
import rlp.models.Model
import rlp._

import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.util.Random

class ModelComparison[A](
  models: Vars[Model[A]],
  performanceGap: Int
) {

  type BS[T <: js.Object, B <: JSOptionBuilder[T, _]] = JSOptionBuilder[T,B]

  implicit def intellijIsWeird(a: AxisBuilder) = a.asInstanceOf[BS[Axis, AxisBuilder]]._result
  implicit def intellijIsWeird(a: LayoutBuilder) = a.asInstanceOf[BS[Layout, LayoutBuilder]]._result
  implicit def intellijIsWeird(a: PlotDataBuilder) = a.asInstanceOf[BS[PlotData, PlotDataBuilder]]._result
  implicit def intellijIsWeird(a: PlotMarkerBuilder) = a.asInstanceOf[BS[PlotMarker, PlotMarkerBuilder]]._result
  implicit def intellijIsWeird(a: ConfigBuilder) = a.asInstanceOf[BS[Config, ConfigBuilder]]._result

  private def modelColour(model: Model[A]): (Int, Int, Int) = {
    val rand = new Random(model.id)
    (rand.nextInt(255), rand.nextInt(255), rand.nextInt(255))
  }

  private var graphDiv: html.Div = _
  private var first = true

  @dom
  private lazy val graph: Binding[html.Div] = {
    val plotDiv = {<div id="graph-div"></div>}
    graphDiv = plotDiv

    val layout: Layout = Layout.showlegend(true)
      .xaxis(Axis.title("Games Played"))
      .yaxis(Axis.title("Performance"))

    val data = PlotData
      .set(plotlymode.lines)

    val items: BindingSeq[PlotData] = for (model:Model[A] <- models) yield {
      val hs = model.performanceHistory.bind
      val (r,g,b) = modelColour(model)

      data
        .x(hs.indices.map(performanceGap*_).toJSArray)
        .y(hs.toJSArray)
        .set(plotlymarker.set(plotlycolor.rgb(r,g,b)))
        .name(model.toString)
    }

    val config: Config = Config.displayModeBar(true)

    Plotly.newPlot(plotDiv, items.bind.toJSArray, layout, config)

    plotDiv
  }

  private def pageResized(): Unit = {
    if (graphDiv != null) {
      Plotly.relayout(graphDiv)
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
