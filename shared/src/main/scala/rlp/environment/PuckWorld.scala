package rlp.environment

import rlp.agent.Agent
import rlp.util.Point2D

import scala.util.Random

object PuckWorld {

  val SCREEN_WIDTH = 800.0
  val SCREEN_HEIGHT = 600.0

  val TARGET_RADIUS = 10
  val PUCK_RADIUS = 30
  val BAD_RADIUS = 170

  val WALL_RESTITUTION = 0.5

  val RED_TARGET_SPEED = 10

  val THRUSTER_IMPULSE = 65
  val MAX_SPEED = 150

  val EPISODE_LENGTH = 3000

  case class State(
    px: Double,
    py: Double,
    pvx: Double,
    pvy: Double,
    gx: Double,
    gy: Double,
    rx: Double,
    ry: Double,
    t: Int,
    totalReward: Double
  )

  case class AgentState(
    px: Double,
    py: Double,
    pvx: Double,
    pvy: Double,
    gx: Double,
    gy: Double,
    rx: Double,
    ry: Double,
  )

  object AgentState {

    def fromState(state: State): AgentState = {
      AgentState(
        state.px, state.py,
        state.pvx, state.pvy,
        state.gx, state.gy,
        state.rx, state.ry,
      )
    }
  }

  sealed trait Action
  case object ThrusterLeft extends Action
  case object ThrusterRight extends Action
  case object ThrusterUp extends Action
  case object ThrusterDown extends Action

  type PuckAgent = Agent[AgentState, Action]
}

class PuckWorld(agent: PuckWorld.PuckAgent) extends Environment[PuckWorld.State] {

  import PuckWorld._

  private val random = new Random()

  var p = Point2D(0,0)
  var pv = Point2D(0,0)
  var r = Point2D(0,0)
  var g = Point2D(0,0)
  var t: Int = _
  var totalReward: Double = 0

  reset()

  override def reset(seed: Int): Unit = {
    random.setSeed(seed)
    t = 0
    totalReward = 0

    p = Point2D(
      PUCK_RADIUS + random.nextDouble() * (SCREEN_WIDTH - 2*PUCK_RADIUS),
      PUCK_RADIUS + random.nextDouble() * (SCREEN_HEIGHT - 2*PUCK_RADIUS)
    )

    pv = Point2D(0, 0)
    r = randomTarget()
    g = randomTarget()

    agent.resetEpisode()
  }

  private def randomTarget(): Point2D = {
    Point2D(
      TARGET_RADIUS + random.nextDouble() * (SCREEN_WIDTH - 2*TARGET_RADIUS),
      TARGET_RADIUS + random.nextDouble() * (SCREEN_HEIGHT - 2*TARGET_RADIUS)
    )
  }

  override def setState(state: PuckWorld.State): Unit = {
    p = Point2D(state.px, state.py)
    pv = Point2D(state.pvx, state.pvy)
    r = Point2D(state.rx, state.ry)
    g = Point2D(state.gx, state.gy)
    t = state.t
    totalReward = state.totalReward
  }

  override def getState(): PuckWorld.State = {
    State(p.x, p.y, pv.x, pv.y, g.x, g.y, r.x, r.y, t, totalReward)
  }

  override def step(): Boolean = {

    val impulse = agent.act(AgentState.fromState(getState())) match  {
      case ThrusterUp => Point2D(-1, 0)
      case ThrusterDown => Point2D(1, 0)
      case ThrusterLeft => Point2D(0, -1)
      case ThrusterRight => Point2D(0, 1)
    }

    pv += impulse * THRUSTER_IMPULSE * Environment.DELTA
    pv = pv.norm() * math.min(pv.magnitude(), MAX_SPEED)

    p += pv * Environment.DELTA

    if (p.x < PUCK_RADIUS || p.x + PUCK_RADIUS > SCREEN_WIDTH) {

      p = p.copy(x = math.max(PUCK_RADIUS, math.min(p.x, SCREEN_WIDTH - PUCK_RADIUS)))
      pv = pv.copy(x = -pv.x * WALL_RESTITUTION)
    }

    if (p.y < PUCK_RADIUS || p.y + PUCK_RADIUS > SCREEN_HEIGHT) {

      p = p.copy(y = math.max(PUCK_RADIUS, math.min(p.y, SCREEN_HEIGHT - PUCK_RADIUS)))
      pv = pv.copy(y = -pv.y * WALL_RESTITUTION)
    }

    r += (p - r).norm() * RED_TARGET_SPEED * Environment.DELTA

    val greenDist = p.distance(g)
    val redDist = p.distance(r)
    val normFactor = math.sqrt(SCREEN_WIDTH*SCREEN_WIDTH + SCREEN_HEIGHT*SCREEN_HEIGHT)

    var reward = -greenDist/normFactor +
      (if (redDist < BAD_RADIUS) 2*(redDist - BAD_RADIUS)/BAD_RADIUS else 0)

    if (greenDist < PUCK_RADIUS) {
      g = randomTarget()
      reward += 100
    }

    agent.percept(reward)
    totalReward += reward

    t += 1
    t == EPISODE_LENGTH
  }
}
