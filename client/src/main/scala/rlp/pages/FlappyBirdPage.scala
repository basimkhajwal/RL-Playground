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

  private val gameEnvironment: FlappyBird = new FlappyBird(new UserAgent())

  private var wasSpacePressed: Boolean = false

  private var initialShow: Boolean = true

  override def show(): Unit = {
    super.show()

    if (initialShow) {

      // Register click listener for space key on the game
      keyboardHandler.registerClickListener { key:String =>
        if (key == " ") {
          wasSpacePressed = true
        }
      }

      initialShow = false
    }
  }

  @dom
  override lazy val gameOptions: Binding[Div] = {
    <div>
      <br/>
      <br/>
      <h5 class="center-align">Controls</h5>
      <br/>
      <h6 class="center-align"><strong>Space</strong> - launch bird</h6>
      <br/>
    </div>
  }

  override protected def createEnvironment(model: AgentPresenter[FlappyBirdAgent]): Environment[State] = {
    new FlappyBird(model.agent)
  }

  override protected def render(ctx: CanvasRenderingContext2D): Unit = {

    ctx.save()

    // Transform the coordinate system
    ctx.scale(ctx.canvas.width / SCREEN_WIDTH, ctx.canvas.height / SCREEN_HEIGHT)

    // Clear the screen to the sky colour
    ctx.fillStyle = "lightblue"
    ctx.fillRect(0, 0, SCREEN_WIDTH, SCREEN_HEIGHT)

    // Render either the game or the training environment
    if (!renderTraining.get) {

      if (gameEnvironment.step()) {
        gameEnvironment.reset()
      }
      renderState(ctx, gameEnvironment.getState())

    } else if (trainingEnvironment != null) {

      renderState(ctx, trainingEnvironment.getState())
    }

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

  /**
    * User agent which acts whenever the user
    * presses space
    */
  class UserAgent extends FlappyBirdAgent {

    override def act(state: AgentState): Action = {
      if (wasSpacePressed) {
        wasSpacePressed = false
        JumpAction
      } else {
        NoAction
      }
    }
  }
}
