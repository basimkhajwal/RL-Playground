package rlp.pages

import com.definitelyscala.plotlyjs._
import com.definitelyscala.plotlyjs.all._
import com.definitelyscala.plotlyjs.PlotlyImplicits._
import com.definitelyscala.plotlyjs.plotlyConts._
import com.thoughtworks.binding.Binding.Vars
import com.thoughtworks.binding.{Binding, dom}
import org.querki.jsext.JSOptionBuilder
import org.scalajs.dom.html
import rlp.models.Model
import rlp._

import scala.scalajs.js

class ModelComparison[A](
  models: Vars[Model[A]]
) {

  type BS[T <: js.Object, B <: JSOptionBuilder[T, _]] = JSOptionBuilder[T,B]

  implicit def intellijIsWeird(a: AxisBuilder) = a.asInstanceOf[BS[Axis, AxisBuilder]]._result
  implicit def intellijIsWeird(a: LayoutBuilder) = a.asInstanceOf[BS[Layout, LayoutBuilder]]._result
  implicit def intellijIsWeird(a: PlotDataBuilder) = a.asInstanceOf[BS[PlotData, PlotDataBuilder]]._result
  implicit def intellijIsWeird(a: PlotMarkerBuilder) = a.asInstanceOf[BS[PlotMarker, PlotMarkerBuilder]]._result
  implicit def intellijIsWeird(a: ConfigBuilder) = a.asInstanceOf[BS[Config, ConfigBuilder]]._result

  @dom
  lazy val content: Binding[html.Div] = {
    <div class="card-panel" id="model-comparison">
      <span class="card-title">Model Comparison</span>
      {
        val plotDiv = {<div></div>}

        val layout: Layout = Layout.title("My line plot")
          .showlegend(true)
          .xaxis(Axis.title("Time"))
          .yaxis(Axis.title("Production"))

        val data = PlotData
          .set(plotlymode.markers.lines)
          .set(plotlymarker.set(plotlysymbol.square))

        val data1: PlotData = data
          .x(js.Array(1,2,3))
          .y(js.Array(1,2,3).map(2*_))
          .set(plotlymarker.size(12.0).set(plotlycolor.rgb(180,0,0)))
          .name("Reds")

        val data2: PlotData = data
          .x(js.Array(1,2,3))
          .y(js.Array(1,2,3))
          .set(plotlymarker.size(10.0).set(plotlycolor.rgb(0, 136, 170)).set(plotlysymbol.cross))
          .name("Blues")

        val config: Config = Config.displayModeBar(false)

        val plot = Plotly.newPlot(plotDiv, js.Array(data1, data2), layout, config)

        plotDiv
      }

    </div>
  }
}
