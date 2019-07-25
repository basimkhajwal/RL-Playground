package rlp.pages

import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.html.Div
import org.scalajs.dom.raw.CanvasRenderingContext2D
import rlp.agent.PolicyNetworkAgent.PolicyNetworkSpace
import rlp.agent.QNetworkAgent.QNetworkSpace
import rlp.agent.QTableAgent.QStateSpace
import rlp.environment.FlappyBird.FlappyBirdAgent
import rlp.environment.{Environment, FlappyBird}
import rlp.presenters._

/**
  * Game page for the flappy bird environment
  */
class FlappyBirdPage extends GamePage[FlappyBird.State, FlappyBirdAgent] {

  import FlappyBird._

  override val name: String = "Flappy Bird"
  override val gameDescription: String =
    "Flappy bird is a single-player side-scrolling game with the objective to travel as far as possible" +
      " whilst avoiding collisions, and the only control being able to jump."

  override val inputDescription: String =
    "Bird height, vertical speed, distance to next gap, height of next gap"

  override val actionDescription: String = "Jump, No Action"
  override val rewardDescription: String =
    "+1 spread across passing each gap, -1000 for hitting the top, bottom or a pipe"

  override protected val MAX_EPISODE_LENGTH: Int = 5000

  override protected val presenterBuilders: List[(String, () => AgentPresenter[FlappyBirdAgent])] = List(
    QTablePresenter.builder(name,
      2, { a => if (a == 0) NoAction else JumpAction },
      AgentParam("Height", QStateSpace.boxed[AgentState](0, SCREEN_HEIGHT - GROUND_HEIGHT, 20, _.y)),
      AgentParam("Vertical Speed", QStateSpace.boxed[AgentState](-MAX_SPEED, MAX_SPEED, 10, _.vy)),
      AgentParam("Next Block Distance", QStateSpace.boxed[AgentState](0, BLOCK_SPACING + BLOCK_WIDTH, 30, _.blockDist)),
      AgentParam("Gap Height", QStateSpace.boxed[AgentState](0, SCREEN_HEIGHT - GROUND_HEIGHT, 20, _.gapMid)),
    ),

    QNetworkPresenter.builder(name,
      2, { a => if (a == 0) NoAction else JumpAction },
      AgentParam("Height", QNetworkSpace.bounded[AgentState](0, SCREEN_HEIGHT - GROUND_HEIGHT, _.y)),
      AgentParam("Vertical Speed", QNetworkSpace.bounded[AgentState](-MAX_SPEED, MAX_SPEED, _.vy)),
      AgentParam("Next Block Distance", QNetworkSpace.bounded[AgentState](0, BLOCK_SPACING + BLOCK_WIDTH, _.blockDist)),
      AgentParam("Gap Height", QNetworkSpace.bounded[AgentState](0, SCREEN_HEIGHT - GROUND_HEIGHT, _.gapMid)),
    ),

    PolicyNetworkPresenter.builder(name,
      2, { a => if (a == 0) NoAction else JumpAction },
      AgentParam("Height", PolicyNetworkSpace.bounded[AgentState](0, SCREEN_HEIGHT - GROUND_HEIGHT, _.y)),
      AgentParam("Vertical Speed", PolicyNetworkSpace.bounded[AgentState](-MAX_SPEED, MAX_SPEED, _.vy)),
      AgentParam("Next Block Distance", PolicyNetworkSpace.bounded[AgentState](0, BLOCK_SPACING + BLOCK_WIDTH, _.blockDist)),
      AgentParam("Gap Height", PolicyNetworkSpace.bounded[AgentState](0, SCREEN_HEIGHT - GROUND_HEIGHT, _.gapMid)),
    )
  )

  override protected def createEnvironment(model: AgentPresenter[FlappyBirdAgent]): Environment[State] = {
    new FlappyBird(model.agent)
  }

  override protected def render(ctx: CanvasRenderingContext2D): Unit = {

    if (trainingEnvironment == null) {
      renderDefault(ctx)
      return
    }

    ctx.save()

    // Transform the coordinate system
    ctx.scale(ctx.canvas.width / SCREEN_WIDTH, ctx.canvas.height / SCREEN_HEIGHT)

    // Clear the screen to the sky colour
    ctx.fillStyle = "lightblue"
    ctx.fillRect(0, 0, SCREEN_WIDTH, SCREEN_HEIGHT)

    renderState(ctx, trainingEnvironment.getState())

    ctx.restore()
  }

  private def renderState(ctx: CanvasRenderingContext2D, state: State): Unit = {

    // Background fill
    ctx.fillStyle = "brown"
    ctx.fillRect(0, SCREEN_HEIGHT - GROUND_HEIGHT, SCREEN_WIDTH, GROUND_HEIGHT)

    ctx.translate(SCREEN_WIDTH / 2 - state.x, 0)

    // Draw each block
    ctx.fillStyle = "lime"
    for ((blockX, gapY) <- state.blocks) {
      ctx.fillRect(blockX, 0, BLOCK_WIDTH, gapY)
      ctx.fillRect(blockX, gapY + GAP_HEIGHT, BLOCK_WIDTH, SCREEN_HEIGHT - GROUND_HEIGHT - (gapY + GAP_HEIGHT))
    }

    // Draw the bird
    ctx.fillStyle = "red"
    ctx.fillRect(state.x, state.y, BIRD_WIDTH, BIRD_HEIGHT)
  }

  /**
    * Evaluate the agent by measuring how far it travels on average
    * over 10 test runs
    *
    * @param model
    * @return
    */
  override protected def agentPerformance(model: AgentPresenter[FlappyBirdAgent]): Double = {
    val testAgent = model.agent.clone()
    val testEnv = new FlappyBird(testAgent)
    val testRuns = 10
    var totalDistance = 0.0

    testAgent.setTrainEnabled(false)

    for (_ <- 0 until testRuns) {
      testEnv.reset()

      var steps = 0
      while (!testEnv.step() && steps < MAX_EPISODE_LENGTH) {
        steps += 1
      }

      totalDistance += testEnv.getState().x
    }

    totalDistance / testRuns
  }
}
