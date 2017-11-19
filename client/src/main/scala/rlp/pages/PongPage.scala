package rlp.pages

import rlp._
import rlp.environment.{Agent, NaivePongAgent, Pong}
import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.html.Div
import rlp.controllers.{QStateSpace, QTableController}


class PongPage extends GamePage[Agent[Pong.AgentState, Pong.Action]] {

  import Pong._

  private var learningAgent: PongAgent = _
  private var trainingEnvironment: Pong = _
  private var gameEnvironment: Pong = _

  private val MAX_EPISODE_LENGTH = 1000
  private var episodeLength = 0

  private val agentNames = List("Rule-based computer", "Computer AI", "Player (WASD)", "Player (Up/Down)")

  private val agentCreators: List[() => PongAgent] = List(
    { () => new NaivePongAgent() },
    { () => learningAgent.clone() },
    { () => new PongUserAgent("w", "s") },
    { () => new PongUserAgent("ArrowUp", "ArrowDown") }
  )

  val leftAgentSelect = new SelectHandler("Player 1", agentNames, renderTraining)
  val rightAgentSelect = new SelectHandler("Player 2", agentNames, renderTraining)

  override val modelControllers = List(
    new QTableController(
      2, { a => if (a == 0) UpAction else DownAction },
      QStateSpace.boxed[AgentState]("Ball X", 0, SCREEN_WIDTH, 20, _.ballPos.x),
      QStateSpace.boxed[AgentState]("Ball Y", 0, SCREEN_HEIGHT, 10, _.ballPos.y),
      QStateSpace.boxed[AgentState]("Paddle Y", 0, SCREEN_HEIGHT, 10, _.currentPaddle),
      QStateSpace.boxed[AgentState]("Ball Angle", 0, 2 * Math.PI, 10, _.ballDir.angle(), false),
      QStateSpace.boxed[AgentState]("Opponent Y", 0, SCREEN_HEIGHT, 10, _.otherPaddle, false),
    )
  )

  override def initTraining(): Unit = {
    val modelController = modelControllers(modelIdx.get)
    learningAgent = modelController.buildAgent()

    trainingEnvironment = new Pong(learningAgent, learningAgent.clone())
  }

  override protected def trainStep(): Unit = {
    episodeLength += 1
    if (trainingEnvironment.step() || episodeLength > MAX_EPISODE_LENGTH) {
      if (episodeLength <= MAX_EPISODE_LENGTH) {
        gameCount := gameCount.get + 1
      }
      trainingEnvironment.reset()
      episodeLength = 0
    }
  }

  private def createGameEnvironment(leftAgentIdx: Int, rightAgentIdx: Int): Unit = {
    val leftAgent = agentCreators(leftAgentIdx)()
    val rightAgent = agentCreators(rightAgentIdx)()

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

