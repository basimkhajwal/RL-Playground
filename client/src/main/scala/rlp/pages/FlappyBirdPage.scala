package rlp.pages

import org.scalajs.dom.raw.CanvasRenderingContext2D
import rlp.agent.QNetworkAgent.QNetworkSpace
import rlp.agent.QStateSpace
import rlp.environment.{Environment, FlappyBird}
import rlp.models.{ModelParam, QNetworkModel, QTableModel}
import rlp.presenters.{AgentPresenter, QNetworkPresenter, QTablePresenter}

class FlappyBirdPage extends GamePage[FlappyBird.State, FlappyBird.FlappyBirdAgent]{

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

  override protected val modelBuilders: List[(String, () => AgentPresenter[FlappyBirdAgent])] = List(
    QTablePresenter.builder(name,
      2, { a => if (a == 0) NoAction else JumpAction },
      ModelParam("Height", QStateSpace.boxed[AgentState](0, SCREEN_HEIGHT - GROUND_HEIGHT, 20, _.y)),
      ModelParam("Vertical Speed", QStateSpace.boxed[AgentState](-MAX_SPEED, MAX_SPEED, 10, _.vy)),
      ModelParam("Next Block Distance", QStateSpace.boxed[AgentState](0, BLOCK_SPACING + BLOCK_WIDTH, 30, _.blockDist)),
      ModelParam("Gap Height", QStateSpace.boxed[AgentState](0, SCREEN_HEIGHT - GROUND_HEIGHT, 20, _.gapMid)),
    ),
    QNetworkPresenter.builder(name,
      2, { a => if (a == 0) NoAction else JumpAction },
      ModelParam("Height", QNetworkSpace.bounded[AgentState](0, SCREEN_HEIGHT - GROUND_HEIGHT, _.y)),
      ModelParam("Vertical Speed", QNetworkSpace.bounded[AgentState](-MAX_SPEED, MAX_SPEED,  _.vy)),
      ModelParam("Next Block Distance", QNetworkSpace.bounded[AgentState](0, BLOCK_SPACING + BLOCK_WIDTH,  _.blockDist)),
      ModelParam("Gap Height", QNetworkSpace.bounded[AgentState](0, SCREEN_HEIGHT - GROUND_HEIGHT, _.gapMid)),
    )
  )

  override protected def createEnvironment(model: AgentPresenter[FlappyBirdAgent]): Environment[FlappyBird.State] = {
    new FlappyBird(model.agent)
  }

  override protected def render(ctx: CanvasRenderingContext2D): Unit = {

    ctx.save()
    ctx.scale(ctx.canvas.width / SCREEN_WIDTH, ctx.canvas.height / SCREEN_HEIGHT)

    ctx.fillStyle = "lightblue"
    ctx.fillRect(0, 0, SCREEN_WIDTH, SCREEN_HEIGHT)

    if (trainingEnvironment != null) {

      val state = trainingEnvironment.getState()

      ctx.fillStyle = "brown"
      ctx.fillRect(0, SCREEN_HEIGHT - GROUND_HEIGHT, SCREEN_WIDTH, GROUND_HEIGHT)

      ctx.translate(SCREEN_WIDTH/2 - state.x, 0)

      ctx.fillStyle = "lime"
      for ((blockX, gapY) <- state.blocks) {
        ctx.fillRect(blockX, 0, BLOCK_WIDTH, gapY)
        ctx.fillRect(blockX, gapY + GAP_HEIGHT, BLOCK_WIDTH, SCREEN_HEIGHT - GROUND_HEIGHT - (gapY + GAP_HEIGHT))
      }

      ctx.fillStyle = "red"
      ctx.fillRect(state.x, state.y, BIRD_WIDTH, BIRD_HEIGHT)
    }

    ctx.restore()
  }

  override protected def modelPerformance(model: AgentPresenter[FlappyBirdAgent]): Double = {
    val testEnv = new FlappyBird(model.agent.clone())
    val testRuns = 10
    var totalDistance = 0.0

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
