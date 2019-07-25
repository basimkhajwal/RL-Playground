package rlp.pages

import rlp.environment.{NaivePongAgent, Pong}
import org.scalajs.dom.CanvasRenderingContext2D
import rlp.agent.PolicyNetworkAgent.PolicyNetworkSpace
import rlp.agent.QNetworkAgent.QNetworkSpace
import rlp.agent.QTableAgent.QStateSpace
import rlp.environment.Pong.PongAgent
import rlp.presenters._

/**
  * Game page for the pong environment
  */
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

  override val performanceEntryGap: Int = 200

  override val presenterBuilders = List(

    QTablePresenter.builder(
      name,
      2, { a => if (a == 0) UpAction else DownAction },
      AgentParam("Ball X", QStateSpace.boxed[AgentState](0, SCREEN_WIDTH, 20, _.ballPos.x)),
      AgentParam("Ball Y", QStateSpace.boxed[AgentState](0, SCREEN_HEIGHT, 10, _.ballPos.y)),
      AgentParam("Paddle Y", QStateSpace.boxed[AgentState](0, SCREEN_HEIGHT, 10, _.currentPaddle)),
      AgentParam("Ball Angle", QStateSpace.boxed[AgentState](0, 2 * Math.PI, 10, _.ballDir.angle()), false),
      AgentParam("Opponent Y", QStateSpace.boxed[AgentState](0, SCREEN_HEIGHT, 10, _.otherPaddle), false)
    ),

    QNetworkPresenter.builder(
      name,
      2, { a => if (a == 0) UpAction else DownAction },
      AgentParam("Ball X", QNetworkSpace[AgentState](1, s => Array(s.ballPos.x / SCREEN_WIDTH))),
      AgentParam("Ball Y", QNetworkSpace[AgentState](1, s => Array(s.ballPos.y / SCREEN_HEIGHT))),
      AgentParam("Paddle Y", QNetworkSpace[AgentState](1, s => Array(s.currentPaddle / SCREEN_HEIGHT)))
    ),

    PolicyNetworkPresenter.builder(
      name,
      2, { a => if (a == 0) UpAction else DownAction },
      AgentParam("Ball X", PolicyNetworkSpace[AgentState](1, s => Array(s.ballPos.x / SCREEN_WIDTH))),
      AgentParam("Ball Y", PolicyNetworkSpace[AgentState](1, s => Array(s.ballPos.y / SCREEN_HEIGHT))),
      AgentParam("Paddle Y", PolicyNetworkSpace[AgentState](1, s => Array(s.currentPaddle / SCREEN_HEIGHT)))
    )
  )

  /**
    * Average the number of wins of an agent agains the naive
    * agent over 10 runs
    *
    * @param model
    * @return
    */
  override protected def agentPerformance(model: AgentPresenter[PongAgent]): Double = {

    val numRuns = 10
    val naivePongAgent = new NaivePongAgent()
    val testAgent = model.agent.clone()
    val testEnv = new Pong(testAgent, naivePongAgent)

    testAgent.setTrainEnabled(false)

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

  override def createEnvironment(model: AgentPresenter[PongAgent]): Pong = {
    new Pong(model.agent, model.agent.clone())
  }

  override protected def render(ctx: CanvasRenderingContext2D): Unit = {
    if (trainingEnvironment == null) {
      renderDefault(ctx)
    } else {
      renderState(ctx, trainingEnvironment.getState())
    }
  }

  private def renderState(ctx: CanvasRenderingContext2D, state: State): Unit = {
    ctx.save()
    ctx.scale(ctx.canvas.width / Pong.SCREEN_WIDTH, ctx.canvas.height / Pong.SCREEN_HEIGHT)

    // Clear the background
    ctx.fillStyle = "black"
    ctx.fillRect(0, 0, SCREEN_WIDTH, SCREEN_HEIGHT)

    // Draw each paddle
    ctx.fillStyle = "lime"
    ctx.fillRect(
      PADDLE_PADDING, state.leftPaddleY,
      PADDLE_WIDTH, PADDLE_HEIGHT
    )
    ctx.fillRect(
      SCREEN_WIDTH - PADDLE_PADDING - PADDLE_WIDTH, state.rightPaddleY,
      PADDLE_WIDTH, PADDLE_HEIGHT
    )

    // Fill in the ball
    ctx.fillRect(state.ballPos.x, state.ballPos.y, BALL_SIZE, BALL_SIZE)

    ctx.restore()
  }
}
