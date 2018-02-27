package rlp.environment

import rlp.agent.Agent

object MountainCar {

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
    if (v < -0.07) v = -0.07
    if (v > 0.07) v = 0.07

    x += v
    if (x <= -1.2) {
      x = -1.2
      v = 0
    }

    agent.percept(-1)

    x >= 0.6
  }
}
