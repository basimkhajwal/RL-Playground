package rlp.pages

import org.scalajs.dom.raw.CanvasRenderingContext2D
import rlp.agent.QNetworkAgent.QNetworkSpace
import rlp.agent.QStateSpace
import rlp.environment.{Environment, FlappyBird}
import rlp.models.{Model, ModelParam, QNetworkModel, QTableModel}

class FlappyBirdPage extends GamePage[FlappyBird.State, FlappyBird.FlappyBirdAgent]{

  import FlappyBird._

  override val name: String = "Flappy Bird"
  override val gameDescription: String = "TODO: Flappy bird description"
  override val inputDescription: String =
    "Ball coordinates, Ball angle of motion, Paddle height, Opponent paddle height"

  override val actionDescription: String = "Paddle Up, Paddle Down"
  override val rewardDescription: String = "+1 for winning episode, -1 for losing episode"

  override protected val MAX_EPISODE_LENGTH: Int = 5000

  override protected val modelBuilders: List[(String, () => Model[FlappyBirdAgent])] = List(
    QTableModel.builder(name,
      2, { a => if (a == 0) NoAction else JumpAction },
      ModelParam("Height", QStateSpace.boxed[AgentState](0, SCREEN_HEIGHT - GROUND_HEIGHT, 20, _.y)),
      ModelParam("Vertical Speed", QStateSpace.boxed[AgentState](-MAX_SPEED, MAX_SPEED, 10, _.vy)),
      ModelParam("Next Block Distance", QStateSpace.boxed[AgentState](0, BLOCK_SPACING + BLOCK_WIDTH, 30, _.blockDist)),
      ModelParam("Gap Height", QStateSpace.boxed[AgentState](0, SCREEN_HEIGHT - GROUND_HEIGHT, 20, _.gapMid)),
    ),
    QNetworkModel.builder(name,
      2, { a => if (a == 0) NoAction else JumpAction },
      ModelParam("Height", QNetworkSpace.bounded[AgentState](0, SCREEN_HEIGHT - GROUND_HEIGHT, _.y)),
      ModelParam("Vertical Speed", QNetworkSpace.bounded[AgentState](-MAX_SPEED, MAX_SPEED,  _.vy)),
      ModelParam("Next Block Distance", QNetworkSpace.bounded[AgentState](0, BLOCK_SPACING + BLOCK_WIDTH,  _.blockDist)),
      ModelParam("Gap Height", QNetworkSpace.bounded[AgentState](0, SCREEN_HEIGHT - GROUND_HEIGHT, _.gapMid)),
    )
  )

  override protected def createEnvironment(model: Model[FlappyBirdAgent]): Environment[FlappyBird.State] = {
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

  override protected def modelPerformance(model: Model[FlappyBirdAgent]): Double = {
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
