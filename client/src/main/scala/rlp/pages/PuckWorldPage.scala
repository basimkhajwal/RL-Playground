package rlp.pages

import org.scalajs.dom.raw.CanvasRenderingContext2D
import rlp.agent.PolicyNetworkAgent.PolicyNetworkSpace
import rlp.agent.QNetworkAgent.QNetworkSpace
import rlp.environment.{Environment, PuckWorld}
import rlp.presenters.{AgentParam, AgentPresenter, PolicyNetworkPresenter, QNetworkPresenter}

/**
  * Puck world environment game page
  */
class PuckWorldPage extends GamePage[PuckWorld.State, PuckWorld.PuckAgent]{

  import PuckWorld._

  override val name: String = "Puck World"
  override val gameDescription: String =
    "Puck world is a simple control environment where a puck controls its movement " +
      " to move towards the green target (stationary) and away from the red target (chasing after puck)."

  override val inputDescription: String =
    "Puck x-y position, velocity, green target position and red target position"

  override val actionDescription: String = "Direction of thruster: left, up, right or down"
  override val rewardDescription: String =
    "Higher reward closer to green target, large negative reward close to red target"

  override protected val MAX_EPISODE_LENGTH: Int = 5000

  override protected val presenterBuilders: List[(String, () => AgentPresenter[PuckAgent])] = List(
    QNetworkPresenter.builder(name,
      4, { a => if (a == 0) ThrusterLeft else if (a == 1) ThrusterRight else if (a == 2) ThrusterUp else ThrusterDown },
      AgentParam("Puck X", QNetworkSpace.bounded[AgentState](0, SCREEN_WIDTH, _.px)),
      AgentParam("Puck Y", QNetworkSpace.bounded[AgentState](0, SCREEN_HEIGHT,  _.py)),
      AgentParam("Velocity X", QNetworkSpace.bounded[AgentState](0, SCREEN_WIDTH, _.pvx)),
      AgentParam("Velocity Y", QNetworkSpace.bounded[AgentState](0, SCREEN_HEIGHT,  _.pvy)),
      AgentParam("Green X", QNetworkSpace.bounded[AgentState](0, SCREEN_WIDTH, _.gx)),
      AgentParam("Green Y", QNetworkSpace.bounded[AgentState](0, SCREEN_HEIGHT,  _.gy)),
      AgentParam("Red X", QNetworkSpace.bounded[AgentState](0, SCREEN_WIDTH, _.rx)),
      AgentParam("Red Y", QNetworkSpace.bounded[AgentState](0, SCREEN_HEIGHT,  _.ry)),
    ),

    PolicyNetworkPresenter.builder(name,
      4, { a => if (a == 0) ThrusterLeft else if (a == 1) ThrusterRight else if (a == 2) ThrusterUp else ThrusterDown },
      AgentParam("Puck X", PolicyNetworkSpace.bounded[AgentState](0, SCREEN_WIDTH, _.px)),
      AgentParam("Puck Y", PolicyNetworkSpace.bounded[AgentState](0, SCREEN_HEIGHT,  _.py)),
      AgentParam("Velocity X", PolicyNetworkSpace.bounded[AgentState](0, SCREEN_WIDTH, _.pvx)),
      AgentParam("Velocity Y", PolicyNetworkSpace.bounded[AgentState](0, SCREEN_HEIGHT,  _.pvy)),
      AgentParam("Green X", PolicyNetworkSpace.bounded[AgentState](0, SCREEN_WIDTH, _.gx)),
      AgentParam("Green Y", PolicyNetworkSpace.bounded[AgentState](0, SCREEN_HEIGHT,  _.gy)),
      AgentParam("Red X", PolicyNetworkSpace.bounded[AgentState](0, SCREEN_WIDTH, _.rx)),
      AgentParam("Red Y", PolicyNetworkSpace.bounded[AgentState](0, SCREEN_HEIGHT,  _.ry)),
    )
  )

  override protected def createEnvironment(model: AgentPresenter[PuckAgent]): Environment[State] = {
    new PuckWorld(model.agent)
  }

  override protected def render(ctx: CanvasRenderingContext2D): Unit = {

    if (trainingEnvironment == null) {
      renderDefault(ctx)
      return
    }

    ctx.save()
    ctx.scale(ctx.canvas.width / SCREEN_WIDTH, ctx.canvas.height / SCREEN_HEIGHT)

    ctx.fillStyle = "white"
    ctx.fillRect(0, 0, SCREEN_WIDTH, SCREEN_HEIGHT)

    val state = trainingEnvironment.getState()

    ctx.strokeStyle = "black"
    ctx.globalAlpha = 0.5

    // Render each puck as a circle, coloured and with a particular radius

    ctx.fillStyle = "#cccccc"
    ctx.beginPath()
    ctx.arc(state.px, state.py, PUCK_RADIUS, 0, 2*math.Pi)
    ctx.fill()
    ctx.stroke()

    ctx.fillStyle = "#11aa22"
    ctx.beginPath()
    ctx.arc(state.gx, state.gy, TARGET_RADIUS, 0, 2*math.Pi)
    ctx.fill()
    ctx.stroke()

    ctx.fillStyle = "#aa1122"
    ctx.beginPath()
    ctx.arc(state.rx, state.ry, TARGET_RADIUS, 0, 2*math.Pi)
    ctx.fill()
    ctx.stroke()

    ctx.fillStyle = "#aa0000"
    ctx.globalAlpha = 0.05
    ctx.beginPath()
    ctx.arc(state.rx, state.ry, BAD_RADIUS, 0, 2*math.Pi)
    ctx.fill()
    ctx.stroke()

    ctx.restore()
  }

  /**
    * Measure the total reward experienced by an agent
    * averaged over 10 episodes
    *
    * @param model
    * @return
    */
  override protected def agentPerformance(model: AgentPresenter[PuckAgent]): Double = {
    val testAgent = model.agent.clone()
    val testEnv = new PuckWorld(testAgent)
    val testRuns = 10
    var totalReward = 0.0

    testAgent.setTrainEnabled(false)

    for (_ <- 0 until testRuns) {
      testEnv.reset()
      var steps = 0

      while (!testEnv.step() && steps < MAX_EPISODE_LENGTH) {
        steps += 1
      }

      totalReward += testEnv.totalReward / steps
    }

    totalReward / testRuns
  }

}
