package rlp.environment

import rlp.environment.Pong.NoAction

import scala.util.Random

object Pong {
  val SCREEN_WIDTH = 800.0
  val SCREEN_HEIGHT = 600.0

  val PADDLE_PADDING = 50.0
  val PADDLE_WIDTH = 30.0
  val PADDLE_HEIGHT = 100.0

  val BALL_SIZE = 20.0

  val BALL_SPEED = 200 * Environment.DELTA
  val PADDLE_SPEED = 100 * Environment.DELTA

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
    def fromState(state: State, isLeft: Boolean): AgentState =
      AgentState(
        state.ballPos, state.ballDir,
        if (isLeft) state.leftPaddleY else state.rightPaddleY,
        if (isLeft) state.rightPaddleY else state.leftPaddleY
      )
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
    ballDir = Point2D.fromPolar(ran.nextDouble()*math.Pi*2, 1)
    leftPaddleY = (SCREEN_HEIGHT - PADDLE_HEIGHT) / 2
    rightPaddleY = leftPaddleY
  }

  def step(): Unit = {
    val s = getState()
    val leftS = AgentState.fromState(s, true)
    val rightS = AgentState.fromState(s, false)


    leftAgent.act(leftS) match {
      case NoAction =>
      case UpAction => leftPaddleY += PADDLE_SPEED
      case DownAction => leftPaddleY -= PADDLE_SPEED
    }
    leftPaddleY = math.min(math.max(0, leftPaddleY), SCREEN_HEIGHT - PADDLE_HEIGHT)

    rightAgent.act(rightS) match {
      case NoAction =>
      case UpAction => rightPaddleY += PADDLE_SPEED
      case DownAction => rightPaddleY -= PADDLE_SPEED
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
      rightWon = ballPos.y < leftPaddleY || ballPos.y > leftPaddleY + PADDLE_HEIGHT
      ballPos = ballPos copy (x = leftDist + (leftDist - ballPos.x))
      ballDir = ballDir copy (x = -ballDir.x)

    } else if (ballPos.x > rightDist) {
      leftWon = ballPos.y < leftPaddleY || ballPos.y > leftPaddleY + PADDLE_HEIGHT
      ballPos = ballPos copy (x = rightDist - (ballPos.x - rightDist))
      ballDir = ballDir copy (x = -ballDir.x)
    }

    val s2 = getState()
    val leftS2 = AgentState.fromState(s2, true)
    val rightS2 = AgentState.fromState(s2, false)

    val (leftReward, rightReward) =
      if (leftWon) (1, -1)
      else if (rightWon) (-1, 1)
      else (0, 0)

    leftAgent.percept(leftS, leftS2, leftReward)
    rightAgent.percept(rightS, rightS2, rightReward)
  }
}

class NaivePongAgent extends Agent[Pong.AgentState, Pong.Action] {

  import Pong._

  override def act(state: AgentState): Action = {
    val y = state.currentPaddle + PADDLE_HEIGHT/2
    val by = state.ballPos.y

    if (by > y+5) Pong.UpAction
    else if (by < y-5) Pong.DownAction
    else NoAction
  }

}

case class Point2D(x: Double, y: Double) {
  def +(that: Point2D) = Point2D(x + that.x, y + that.y)
  def -(that: Point2D) = Point2D(x - that.x, y - that.y)
  def *(scale: Double) = Point2D(x * scale, y * scale)
}
object Point2D {
  def fromPolar(theta: Double, m: Double): Point2D = Point2D(m*math.cos(theta), m*math.sin(theta))
}
