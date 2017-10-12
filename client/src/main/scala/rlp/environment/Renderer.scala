package rlp.environment

import org.scalajs.dom._

trait Renderer[S] {
  def render(state: S, ctx: CanvasRenderingContext2D): Unit
}

class PongRenderer extends Renderer[Pong.State] {

  import Pong._

  override def render(state: State, ctx: CanvasRenderingContext2D): Unit = {

    //ctx.save()
    //ctx.scale(ctx.canvas.width / Pong.SCREEN_WIDTH, -ctx.canvas.height / Pong.SCREEN_HEIGHT)

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

    //ctx.restore()
  }
}
