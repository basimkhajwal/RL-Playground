package rlp.environment

import rlp.agent.Agent

import scala.util.Random
import rlp.util.Point2D

object Pong {
  val SCREEN_WIDTH = 800.0
  val SCREEN_HEIGHT = 600.0

  val PADDLE_PADDING = 50.0
  val PADDLE_WIDTH = 30.0
  val PADDLE_HEIGHT = 100.0

  val BALL_SIZE = 20.0

  val BALL_SPEED = 400 * Environment.DELTA
  val PADDLE_SPEED = 250 * Environment.DELTA

  sealed trait Action
  object NoAction extends Action
  object UpAction extends Action
  object DownAction extends Action

  case class State(
    ballPos: Point2D, ballDir: Point2D,
    leftPaddleY: Double, rightPaddleY: Double
  )

  case class AgentState(
    ballPos: Point2D, ballDir: Point2D,
    currentPaddle: Double, otherPaddle: Double
  )
  object AgentState {
    def fromState(state: State, isLeft: Boolean): AgentState = {
      if (isLeft) {
        AgentState(state.ballPos, state.ballDir, state.leftPaddleY, state.rightPaddleY)
      } else {
        AgentState(
          state.ballPos.copy(x = SCREEN_WIDTH - state.ballPos.x),
          state.ballDir.copy(x = -state.ballDir.x),
          state.rightPaddleY, state.leftPaddleY
        )
      }
    }
  }

  type PongAgent = Agent[AgentState, Action]
}

class Pong(val leftAgent: Pong.PongAgent, val rightAgent: Pong.PongAgent) extends Environment[Pong.State] {

  import Pong._

  var ballPos: Point2D = _
  var ballDir: Point2D = _
  var leftPaddleY: Double = _
  var rightPaddleY: Double = _
  reset()

  override def setState(state: State): Unit = {
    ballPos = state.ballPos
    ballDir = state.ballDir
    leftPaddleY = state.leftPaddleY
    rightPaddleY = state.rightPaddleY
  }

  override def getState() = State(ballPos, ballDir, leftPaddleY, rightPaddleY)

  override def reset(seed: Int): Unit = {
    val ran = new Random(seed)
    ballPos = Point2D((SCREEN_WIDTH - BALL_SIZE) / 2, (SCREEN_HEIGHT - BALL_SIZE) / 2)
    ballDir = Point2D.fromPolar(ran.nextDouble() * 2 * math.Pi, 1)
    leftPaddleY = (SCREEN_HEIGHT - PADDLE_HEIGHT) / 2
    rightPaddleY = leftPaddleY

    leftAgent.resetEpisode()
    rightAgent.resetEpisode()
  }

  override def step(): Boolean = {
    val prevState = getState()

    leftAgent.act(AgentState.fromState(prevState, true)) match {
      case NoAction =>
      case DownAction => leftPaddleY += PADDLE_SPEED
      case UpAction => leftPaddleY -= PADDLE_SPEED
    }
    leftPaddleY = math.min(math.max(0, leftPaddleY), SCREEN_HEIGHT - PADDLE_HEIGHT)

    rightAgent.act(AgentState.fromState(prevState, false)) match {
      case NoAction =>
      case DownAction => rightPaddleY += PADDLE_SPEED
      case UpAction => rightPaddleY -= PADDLE_SPEED
    }
    rightPaddleY = math.min(math.max(0, rightPaddleY), SCREEN_HEIGHT - PADDLE_HEIGHT)

    ballPos += ballDir * BALL_SPEED

    if (ballPos.y < 0 || ballPos.y + BALL_SIZE > SCREEN_HEIGHT) {
      val newY =
        if (ballPos.y < 0) -ballPos.y
        else ballPos.y - (ballPos.y + BALL_SIZE - SCREEN_HEIGHT)

      ballPos = ballPos copy (y = newY)
      ballDir = ballDir copy (y = -ballDir.y)
    }

    val leftDist = PADDLE_PADDING + PADDLE_WIDTH
    val rightDist = SCREEN_WIDTH - leftDist - BALL_SIZE

    var leftWon = false
    var rightWon = false

    if (ballPos.x < leftDist) {
      rightWon = ballPos.y + BALL_SIZE < leftPaddleY || ballPos.y > leftPaddleY + PADDLE_HEIGHT
      ballPos = ballPos copy (x = leftDist + (leftDist - ballPos.x))
      ballDir = ballDir copy (x = -ballDir.x)

    } else if (ballPos.x > rightDist) {
      leftWon = ballPos.y + BALL_SIZE < rightPaddleY || ballPos.y > rightPaddleY + PADDLE_HEIGHT
      ballPos = ballPos copy (x = rightDist - (ballPos.x - rightDist))
      ballDir = ballDir copy (x = -ballDir.x)
    }

    val (leftReward, rightReward) =
      if (leftWon) (1, -1)
      else if (rightWon) (-1, 1)
      else (0, 0)

    leftAgent.percept(leftReward)
    rightAgent.percept(rightReward)

    leftWon || rightWon
  }
}

class NaivePongAgent extends Agent[Pong.AgentState, Pong.Action] {

  import Pong._

  private var won: Boolean = false

  override def act(state: AgentState): Action = {
    val y = state.currentPaddle + PADDLE_HEIGHT/2
    val by = state.ballPos.y

    if (by > y+5) DownAction
    else if (by < y-5) UpAction
    else NoAction
  }

  def hasWon() = won

  override def percept(reward: Double): Unit = {
    won = reward > 0
  }
}
