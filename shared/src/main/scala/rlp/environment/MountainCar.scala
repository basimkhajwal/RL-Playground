package rlp.environment

import rlp.agent.Agent

object MountainCar {

  val MIN_X = -1.2
  val MAX_X = 0.6

  val MIN_Y = 0
  val MAX_Y = 1.3

  val MAX_SPEED = 0.07
  val GOAL_X = 0.5

  val CAR_WIDTH = 40
  val CAR_HEIGHT = 20

  def positionHeight(x: Double): Double = {
    math.sin(3 * x)*0.45 + 0.55
  }

  case class State(v: Double, x: Double)

  sealed trait Action
  case object LeftAction extends Action
  case object RightAction extends Action
  case object NoAction extends Action

  type MountainCarAgent = Agent[State, Action]
}

class MountainCar(val agent: MountainCar.MountainCarAgent) extends Environment[MountainCar.State] {

  import MountainCar._

  var v: Double = _
  var x: Double = _
  reset()

  override def reset(seed: Int): Unit = {
    v = 0
    x = -0.5
    agent.resetEpisode()
  }

  override def setState(state: State): Unit = {
    v = state.v
    x = state.x
  }

  override def getState(): State = State(v, x)

  override def step(): Boolean = {

    val action = agent.act(getState()) match {
      case NoAction => 0
      case LeftAction => -1
      case RightAction => 1
    }

    v += action * 0.001 - math.cos(3 * x) * 0.0025
    if (v < -MAX_SPEED) v = -MAX_SPEED
    if (v > MAX_SPEED) v = MAX_SPEED

    x += v
    if (x <= MIN_X) {
      x = MIN_X
      v = 0
    }

    agent.percept(-1)

    x >= MAX_X
  }
}
