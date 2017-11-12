package rlp.pages


import com.thoughtworks.binding.Binding.Var
import rlp._
import rlp.environment.{Agent, Environment, NaivePongAgent, Pong}
import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.{CanvasRenderingContext2D, document, html}
import org.scalajs.dom.html.{Canvas, Div}
import org.scalajs.dom.raw.Event
import rlp.controllers.{ModelController, QTableController, QStateSpace}

import scala.scalajs.js.{Date, timers}

class PongPage extends GamePage[Agent[Pong.AgentState, Pong.Action]] {

  import Pong._

  private var environment: Pong = _

  override val modelControllers = List(
    new QTableController(
      2, { a => if (a == 0) UpAction else DownAction },
      QStateSpace.boxed[AgentState]("Ball X", 0, SCREEN_WIDTH, 10, _.ballPos.x),
      QStateSpace.boxed[AgentState]("Ball Y", 0, SCREEN_HEIGHT, 10, _.ballPos.y),
      QStateSpace.boxed[AgentState]("Paddle Y", 0, SCREEN_HEIGHT, 10, _.currentPaddle)
    )
  )

  override def initTraining(): Unit = {
    val modelController = modelControllers(modelIdx.get)
    val learningAgent = modelController.buildAgent()

    environment = new Pong(learningAgent, learningAgent.clone())
  }

  override protected def trainStep(): Unit = {
    if (environment.step()) {
      environment.reset()
    }
  }

  override protected def render(ctx: CanvasRenderingContext2D): Unit = {

    if (environment == null) {
      ctx.clearRect(0, 0, SCREEN_WIDTH, SCREEN_HEIGHT)
      return
    }

    val state = environment.getState()

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
  override lazy val gameOptions: Binding[Div] = {
    <div>
    </div>
  }
}

