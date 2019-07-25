package rlp.pages

import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.html.Div
import org.scalajs.dom.raw.CanvasRenderingContext2D
import rlp.agent.PolicyNetworkAgent.PolicyNetworkSpace
import rlp.agent.QNetworkAgent.QNetworkSpace
import rlp.agent.QTableAgent.QStateSpace
import rlp.environment.{Environment, MountainCar}
import rlp.presenters._

/**
  * Game page for the mountain car environment
  */
class MountainCarPage extends GamePage[MountainCar.State, MountainCar.MountainCarAgent] {

  import MountainCar._

  override val name: String = "Mountain Car"
  override val gameDescription: String =
    "A classic reinforcement learning problem in which a car must learn to overcome" +
      " gravity and escape the hill despite not having a powerful enough engine."

  override val inputDescription: String =
    "Car position, car velocity"

  override val actionDescription: String = "Apply engine left, Apply engine right, No action"
  override val rewardDescription: String =
    "-1 for every time step"

  override protected val MAX_EPISODE_LENGTH: Int = 2000

  override protected val presenterBuilders: List[(String, () => AgentPresenter[MountainCarAgent])] = List(
    QTablePresenter.builder(name,
      3, { a => if (a == 0) NoAction else if (a == 1) LeftAction else RightAction },
      AgentParam("X position", QStateSpace.boxed[State](-1.2, 0.6, 50, _.x)),
      AgentParam("Velocity", QStateSpace.boxed[State](-0.07, 0.07, 50, _.v)),
    ),

    QNetworkPresenter.builder(name,
      3, { a => if (a == 0) NoAction else if (a == 1) LeftAction else RightAction },
      AgentParam("X position", QNetworkSpace.bounded[State](-1.2, 0.6, _.x)),
      AgentParam("Velocity", QNetworkSpace.bounded[State](-0.07, 0.07, _.v)),
    ),

    PolicyNetworkPresenter.builder(name,
      3, { a => if (a == 0) NoAction else if (a == 1) LeftAction else RightAction },
      AgentParam("X position", PolicyNetworkSpace.bounded[State](-1.2, 0.6, _.x)),
      AgentParam("Velocity", PolicyNetworkSpace.bounded[State](-0.07, 0.07, _.v)),
    )
  )

  override protected def createEnvironment(model: AgentPresenter[MountainCarAgent]): Environment[State] = {
    new MountainCar(model.agent)
  }

  override protected def render(ctx: CanvasRenderingContext2D): Unit = {

    if (trainingEnvironment == null) {
      renderDefault(ctx)
      return
    }

    ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height)

    ctx.save()

    // Transform the coordinate system
    ctx.scale(ctx.canvas.width / (MAX_X - MIN_X), -ctx.canvas.height / (MAX_Y - MIN_Y))
    ctx.translate(-MIN_X, -(MAX_Y - MIN_Y))

    ctx.fillStyle = "white"
    ctx.fillRect(MIN_X, MIN_Y, MAX_X, MAX_Y)

    renderState(ctx, trainingEnvironment.getState())

    ctx.restore()
  }

  private def renderState(ctx: CanvasRenderingContext2D, state: State): Unit = {

    // Stroke the curved line of the mountains gradient

    ctx.fillStyle = "black"
    ctx.strokeStyle = "black"
    ctx.lineWidth = 0.01

    ctx.beginPath()

    val xs = MIN_X to MAX_X by ((MAX_X-MIN_X) / 100)
    val ys = xs map positionHeight
    ctx.moveTo(xs(0), ys(0))
    for ((x,y) <- xs.zip(ys).tail) {
      ctx.lineTo(x, y)
    }

    ctx.stroke()

    // Draw in the goal and the block
    ctx.fillStyle = "green"
    ctx.fillRect(GOAL_X, positionHeight(GOAL_X)-0.05, 0.2, 0.06)
    ctx.fillStyle = "red"
    ctx.fillRect(MIN_X, positionHeight(MIN_X)-0.008, 0.02, 0.2)

    // Draw a circle at the position of the cart
    ctx.fillStyle = "blue"
    ctx.beginPath()
    ctx.arc(state.x, positionHeight(state.x), 0.05, 0, 2*math.Pi)
    ctx.fill()
  }

  /**
    * Measure the agent performance by timing how long it takes
    * to reach the goal on average over ten runs (smaller is better)
    *
    * @param model
    * @return
    */
  override protected def agentPerformance(model: AgentPresenter[MountainCarAgent]): Double = {
    val testAgent = model.agent.clone()
    val testEnv = new MountainCar(testAgent)
    val testRuns = 10
    var totalSteps = 0.0

    testAgent.setTrainEnabled(false)

    for (_ <- 0 until testRuns) {
      testEnv.reset()

      var steps = 0
      while (!testEnv.step() && steps < MAX_EPISODE_LENGTH) {
        steps += 1
      }

      totalSteps += steps
    }

    totalSteps / testRuns
  }
}
