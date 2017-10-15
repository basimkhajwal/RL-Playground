package rlp.pages

import rlp._
import rlp.environment.Pong
import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.{CanvasRenderingContext2D}
import org.scalajs.dom.html.{Canvas, Div}

class PongPage {

  import Pong._

  private def updateCanvas(ctx: CanvasRenderingContext2D, state: State): Unit = {
    ctx.save()
    ctx.scale(ctx.canvas.width / Pong.SCREEN_WIDTH, ctx.canvas.height / Pong.SCREEN_HEIGHT)

    ctx.fillStyle = "black"
    ctx.fillRect(0, 0, SCREEN_WIDTH, SCREEN_HEIGHT)

    ctx.fillStyle = "lime"
    ctx.fillRect(
      PADDLE_PADDING, state.leftPaddleY,
      PADDLE_WIDTH, PADDLE_HEIGHT
    )
    ctx.fillRect(
      SCREEN_WIDTH - PADDLE_PADDING - PADDLE_WIDTH, state.rightPaddleY,
      PADDLE_WIDTH, PADDLE_HEIGHT
    )
    ctx.fillRect(state.ballPos.x, state.ballPos.y, BALL_SIZE, BALL_SIZE)

    ctx.restore()
  }

  @dom
  def content(): Binding[Div] = {

    val canvas: Canvas = <canvas width={800} height={600}></canvas>
    val ctx = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]

    <div id="app">
      <div class="row">

        <div class="col s12">
          <h5 class="white-text center-align">PONG</h5>
        </div>

        <div class="col s8">
          <div class="card-panel orange lighten">
            <h5 class="white-text center-align">Game Container</h5>
            { canvas }
          </div>
        </div>

        <div class="col s4">
          <div class="card-panel red lighten">
            <h5 class="white-text">Controls Container</h5>
            <p>TODO: Add some controls here</p>
          </div>
        </div>
      </div>
    </div>
  }
}
