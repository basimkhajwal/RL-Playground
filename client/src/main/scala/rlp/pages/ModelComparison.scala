package rlp.pages

import com.definitelyscala.plotlyjs._
import com.definitelyscala.plotlyjs.all._
import com.definitelyscala.plotlyjs.PlotlyImplicits._
import com.definitelyscala.plotlyjs.plotlyConts._
import com.thoughtworks.binding.Binding.{BindingSeq, Vars}
import com.thoughtworks.binding.{Binding, dom}
import org.querki.jsext.JSOptionBuilder
import org.scalajs.dom.html
import rlp.models.Model
import rlp._

import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.util.Random

class ModelComparison[A](
  models: Vars[Model[A]]
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

  @dom
  lazy val content: Binding[html.Div] = {
    <div class="card-panel" id="model-comparison">
      <span class="card-title">Model Comparison</span>
      {
        val plotDiv = {<div></div>}

        val layout: Layout = Layout.showlegend(true)
          .xaxis(Axis.title("Games Played"))
          .yaxis(Axis.title("Performance"))

        val data = PlotData
          .set(plotlymode.lines)

        val items: BindingSeq[PlotData] = for (model:Model[A] <- models) yield {
          val hs = model.performanceHistory.bind
          val (r,g,b) = modelColour(model)

          data
            .x(hs.indices.map(100*_).toJSArray)
            .y(hs.toJSArray)
            .set(plotlymarker.set(plotlycolor.rgb(r,g,b)))
            .name(model.toString)
        }

        val config: Config = Config.displayModeBar(false)

        Plotly.newPlot(plotDiv, items.bind.toJSArray, layout, config)

        plotDiv
      }
    </div>
  }
}
