package rlp.pages

import com.thoughtworks.binding.Binding.{BindingSeq, Constants}
import rlp._
import rlp.environment.{NaivePongAgent, Pong}
import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.html.Div
import rlp.agent.QNetworkAgent.QNetworkSpace
import rlp.agent.QStateSpace
import rlp.environment.Pong.PongAgent
import rlp.models.{Model, ModelParam, QNetworkModel, QTableModel}
import rlp.ui.SelectHandler

class PongPage extends GamePage[Pong.State, PongAgent] {

  import Pong._

  override val name: String = "Pong"
  override val gameDescription: String =
    "Pong is a classic arcade game in which two opponents control paddles to hit " +
      "a ball past the other opponent. An episode is terminated once the ball passes either paddle."

  override val inputDescription: String =
    "Ball coordinates, Ball angle of motion, Paddle height, Opponent paddle height"

  override val actionDescription: String = "Paddle Up, Paddle Down"
  override val rewardDescription: String = "+1 for winning episode, -1 for losing episode"

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

  override val performanceEntryGap: Int = 200

  override val modelBuilders = List(

    QTableModel.builder(
      name,
      2, { a => if (a == 0) UpAction else DownAction },
      ModelParam("Ball X", QStateSpace.boxed[AgentState](0, SCREEN_WIDTH, 20, _.ballPos.x)),
      ModelParam("Ball Y", QStateSpace.boxed[AgentState](0, SCREEN_HEIGHT, 10, _.ballPos.y)),
      ModelParam("Paddle Y", QStateSpace.boxed[AgentState](0, SCREEN_HEIGHT, 10, _.currentPaddle)),
      ModelParam("Ball Angle", QStateSpace.boxed[AgentState](0, 2 * Math.PI, 10, _.ballDir.angle()), false),
      ModelParam("Opponent Y", QStateSpace.boxed[AgentState](0, SCREEN_HEIGHT, 10, _.otherPaddle), false)
    ),

    QNetworkModel.builder(
      name,
      2, { a => if (a == 0) UpAction else DownAction },
      ModelParam("Ball X", QNetworkSpace[AgentState](1, s => Array(s.ballPos.x / SCREEN_WIDTH))),
      ModelParam("Ball Y", QNetworkSpace[AgentState](1, s => Array(s.ballPos.y / SCREEN_HEIGHT))),
      ModelParam("Paddle Y", QNetworkSpace[AgentState](1, s => Array(s.currentPaddle / SCREEN_HEIGHT)))
    )
  )

  override protected def modelPerformance(model: Model[PongAgent]): Double = {

    val numRuns = 10
    val naivePongAgent = new NaivePongAgent()
    val testEnv = new Pong(model.agent.clone(), naivePongAgent)

    var totalScore = 0.0

    for (_ <- 0 until numRuns) {

      var episodeLength = 0
      while (!testEnv.step() && episodeLength < MAX_EPISODE_LENGTH) {
        episodeLength += 1
      }

      if (episodeLength < MAX_EPISODE_LENGTH) {
        totalScore += (if (naivePongAgent.hasWon()) -1 else 1)
      }
    }

    totalScore / numRuns
  }

  override def createEnvironment(model: Model[PongAgent]): Pong = {
    new Pong(model.agent, new NaivePongAgent)
  }

  private def createAgent(idx: Int): PongAgent = idx match {
    case 0 => new NaivePongAgent
    case 1 => new PongUserAgent("w", "s")
    case 2 => new PongUserAgent("ArrowUp", "ArrowDown")
    case _ => models.get(idx - 3).agent.clone()
  }

  private def createGameEnvironment(leftAgentIdx: Int, rightAgentIdx: Int): Unit = {
    val leftAgent = createAgent(leftAgentIdx)
    val rightAgent = createAgent(rightAgentIdx)

    gameEnvironment = new Pong(leftAgent, rightAgent)
  }

  override protected def render(ctx: CanvasRenderingContext2D): Unit = {

    if (renderTraining.get) {
      if (trainingEnvironment == null) {
        ctx.clearRect(0, 0, SCREEN_WIDTH, SCREEN_HEIGHT)
      } else {
        renderState(ctx, trainingEnvironment.getState())
      }
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
      <br/>{leftAgentSelect.handler.bind}<br/>{rightAgentSelect.handler.bind}{createGameEnvironment(leftAgentSelect.selectedIndex.bind, rightAgentSelect.selectedIndex.bind)
    ""}
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

