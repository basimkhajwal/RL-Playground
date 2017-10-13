package rlp

import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.html.{Canvas, Div}
import org.scalajs.dom.raw
import org.scalajs.dom.document
import org.scalajs.dom.raw.CanvasRenderingContext2D
import rlp.environment.{Environment, NaivePongAgent, Pong, PongRenderer}

import scala.scalajs.js.timers


object Client {

  implicit def makeIntellijHappy[T<:raw.Node](x: scala.xml.Node): Binding[T] =
    throw new AssertionError("This should never execute.")

  def fn(ctx: CanvasRenderingContext2D, pong: Pong, renderer: PongRenderer): Unit = {
    pong.step()
    renderer.render(pong.getState(), ctx)
  }

  @dom
  def render(): Binding[Div] = {
    val pong = new Pong(new NaivePongAgent(), new NaivePongAgent())
    val renderer = new PongRenderer()
    <div>
      {
        val c: Canvas = { <canvas width={800} height={600}></canvas> }.asInstanceOf[Canvas]
        val ctx = c.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
        timers.setInterval(1000 * Environment.DELTA) { fn(ctx, pong, renderer) }
        c
      }
    </div>
  }

  @dom
  def app(): Binding[Div] = {
    <div id="app">
      <div class="row">

        <div class="col s12">
          <div class="card-panel green darken-2">
            <h5 class="white-text center-align">Game Select</h5>
          </div>
        </div>

        <div class="col s8">
          <div class="card-panel orange lighten">
            <h5 class="white-text center-align">Game Container</h5>
            { render.bind }
          </div>
        </div>

        <div class="col s4">
          <div class="card-panel red lighten">
            <h5 class="white-text">Controls Container</h5>
          </div>
        </div>

        <div class="col s6">
          <div class="card-panel teal lighten">
            <h5 class="white-text">Model Selection</h5>
          </div>
        </div>

        <div class="col s6">
          <div class="card-panel indigo lighten">
            <h5 class="white-text">Model Training</h5>
          </div>
        </div>
      </div>
    </div>
  }

  def main(args: Array[String]): Unit = {
    dom.render(document.getElementById("clientContainer"), app)
  }
}
