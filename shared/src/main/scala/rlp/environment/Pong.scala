package rlp.environment

import scala.util.Random

object Pong {
  val SCREEN_WIDTH = 800
  val SCREEN_HEIGHT = 600

  val PADDLE_PADDING = 50
  val PADDLE_WIDTH = 40
  val PADDLE_HEIGHT = 100

  val BALL_SIZE = 20

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

  type PongAgent = Agent[State, Action, Environment[State]]
}

class Pong(val leftAgent: Pong.PongAgent, val rightAgent: Pong.PongAgent) extends Environment[Pong.State] {

  var ballPos: Point2D = _
  var ballDir: Point2D = _
  var leftPaddleY: Double = _
  var rightPaddleY: Double = _
  reset()

  override def getState() = Pong.State(ballPos, ballDir, leftPaddleY, rightPaddleY)

  override def reset(seed: Int): Unit = {
    val ran = new Random(seed)
    ballPos = Point2D((Pong.SCREEN_WIDTH - Pong.BALL_SIZE) / 2, (Pong.SCREEN_HEIGHT - Pong.BALL_SIZE) / 2)
    ballDir = Point2D.fromPolar(ran.nextDouble()*math.Pi*2, 1)
    leftPaddleY = (Pong.SCREEN_HEIGHT - Pong.PADDLE_HEIGHT) / 2
    rightPaddleY = leftPaddleY
  }

  def step(): Unit = {
    val s = getState()
    leftAgent.act(s) match {
      case Pong.NoAction =>
      case Pong.UpAction => leftPaddleY += Pong.PADDLE_SPEED
      case Pong.DownAction => leftPaddleY -= Pong.PADDLE_SPEED
    }
    rightAgent.act(s) match {
      case Pong.NoAction =>
      case Pong.UpAction => rightPaddleY += Pong.PADDLE_SPEED
      case Pong.DownAction => rightPaddleY -= Pong.PADDLE_SPEED
    }

    ballPos += ballDir * Pong.BALL_SPEED

    if (ballPos.y < 0 || ballPos.y > Pong.SCREEN_HEIGHT) {
      val newY =
        if (ballPos.y < 0) -ballPos.y
        else Pong.SCREEN_HEIGHT - (Pong.SCREEN_HEIGHT - ballPos.y)

      ballPos = ballPos copy (y = newY)
      ballDir = ballDir copy (y = -ballDir.y)
    }

    val leftDist = Pong.PADDLE_PADDING + Pong.PADDLE_WIDTH
    val rightDist = Pong.SCREEN_WIDTH - leftDist

    var leftWon = false
    var rightWon = false

    if (ballPos.x < leftDist) {

      rightWon = ballPos.y < leftPaddleY || ballPos.y > leftPaddleY + Pong.PADDLE_HEIGHT

      ballPos = ballPos copy (x = leftDist + (leftDist - ballPos.x))
      ballDir = ballDir copy (x = -ballDir.x)

    } else if (ballPos.x > rightDist) {

      leftWon = ballPos.y < leftPaddleY || ballPos.y > leftPaddleY + Pong.PADDLE_HEIGHT

      ballPos = ballPos copy (x = rightDist - (ballPos.x - rightDist))
      ballDir = ballDir copy (x = -ballDir.x)
    }

    val s2 = getState()

    if (leftWon) {
      leftAgent.percept(s, s2, 1)
      rightAgent.percept(s, s2, -1)
    } else if (rightWon) {
      leftAgent.percept(s, s2, -1)
      rightAgent.percept(s, s2, 1)
    } else {
      leftAgent.percept(s, s2, 0)
      rightAgent.percept(s, s2, 0)
    }
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
