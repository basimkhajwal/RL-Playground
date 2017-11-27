package rlp.pages

import com.thoughtworks.binding.Binding.{BindingSeq, Constants}
import rlp._
import rlp.environment.{NaivePongAgent, Pong}
import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.html.Div
import rlp.agent.QStateSpace
import rlp.models.{Model, QModelParam, QTableModel}
import rlp.utils.SelectHandler


class PongPage extends GamePage[Pong.State, Pong.PongAgent] {

  import Pong._

  private var gameEnvironment: Pong = _

  private val agentNames: BindingSeq[String] = {
    val defaultNames = Constants("Rule-based computer", "Player (W/S)", "Player (Up/Down)")
    for {
      seq <- Constants(defaultNames, models.map(_.toString))
      x <- seq
    } yield x
  }

  val leftAgentSelect = new SelectHandler("Player 1", agentNames, renderTraining)

  val rightAgentSelect = new SelectHandler("Player 2", agentNames, renderTraining)

  override val modelBuilders = List(
    QTableModel.builder(
      2, { a => if (a == 0) UpAction else DownAction },
      QModelParam("Ball X", QStateSpace.boxed[AgentState](0, SCREEN_WIDTH, 20, _.ballPos.x)),
      QModelParam("Ball Y", QStateSpace.boxed[AgentState](0, SCREEN_HEIGHT, 10, _.ballPos.y)),
      QModelParam("Paddle Y", QStateSpace.boxed[AgentState](0, SCREEN_HEIGHT, 10, _.currentPaddle)),
      QModelParam("Ball Angle", QStateSpace.boxed[AgentState](0, 2 * Math.PI, 10, _.ballDir.angle()), false),
      QModelParam("Opponent Y", QStateSpace.boxed[AgentState](0, SCREEN_HEIGHT, 10, _.otherPaddle), false)
    )
  )

  override def createEnvironment(model: Model[PongAgent]): Pong = {
    new Pong(model.agent, model.agent.clone())
  }

  private def createAgent(idx: Int): PongAgent = idx match {
    case 0 => new NaivePongAgent
    case 1 => new PongUserAgent("w", "s")
    case 2 => new PongUserAgent("ArrowUp", "ArrowDown")
    case _ => models.get(idx-3).agent.clone()
  }

  private def createGameEnvironment(leftAgentIdx: Int, rightAgentIdx: Int): Unit = {
    val leftAgent = createAgent(leftAgentIdx)
    val rightAgent = createAgent(rightAgentIdx)

    gameEnvironment = new Pong(leftAgent, rightAgent)
  }

  override protected def render(ctx: CanvasRenderingContext2D): Unit = {

    if (trainingEnvironment == null) {
      ctx.clearRect(0, 0, SCREEN_WIDTH, SCREEN_HEIGHT)
      return
    }

    if (renderTraining.get) {
      renderState(ctx, trainingEnvironment.getState())
    } else {
      if (gameEnvironment.step()) gameEnvironment.reset()
      renderState(ctx, gameEnvironment.getState())
    }
  }

  private def renderState(ctx: CanvasRenderingContext2D, state: State): Unit = {
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
      <br />
      { leftAgentSelect.handler.bind }
      <br />
      { rightAgentSelect.handler.bind }

      {
        createGameEnvironment(leftAgentSelect.selectedIndex.bind, rightAgentSelect.selectedIndex.bind)
        ""
      }
    </div>
  }

  class PongUserAgent(val upKey: String, val downKey: String) extends PongAgent {

    override def act(state: AgentState): Action = {
      val up = keyboardHandler.isKeyDown(upKey)
      val down = keyboardHandler.isKeyDown(downKey)

      if (up && !down) UpAction
      else if (down && !up) DownAction
      else NoAction
    }
  }
}

