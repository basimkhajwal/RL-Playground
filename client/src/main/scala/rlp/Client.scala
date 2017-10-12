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

  def main(args: Array[String]): Unit = {
    dom.render(document.getElementById("clientContainer"), render)
  }
}
