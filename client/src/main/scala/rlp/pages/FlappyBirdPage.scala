package rlp.pages

import org.scalajs.dom.raw.CanvasRenderingContext2D
import rlp.agent.QStateSpace
import rlp.environment.{Environment, FlappyBird}
import rlp.models.{Model, ModelParam, QTableModel}

class FlappyBirdPage extends GamePage[FlappyBird.State, FlappyBird.FlappyBirdAgent]{

  import FlappyBird._

  override val name: String = "Flappy Bird"
  override val description: String = "TODO: Flappy bird description"

  override protected val modelBuilders: List[(String, () => Model[FlappyBirdAgent])] = List(
    QTableModel.builder(name,
      2, { a => if (a == 0) NoAction else JumpAction },
      ModelParam("Height", QStateSpace.boxed[AgentState](0, SCREEN_HEIGHT - GROUND_HEIGHT, 20, _.y)),
      ModelParam("Vertical Speed", QStateSpace.boxed[AgentState](-MAX_SPEED, MAX_SPEED, 10, _.vy)),
      ModelParam("Next Block Distance", QStateSpace.boxed[AgentState](0, BLOCK_SPACING + BLOCK_WIDTH, 30, _.blockDist)),
      ModelParam("Gap Height", QStateSpace.boxed[AgentState](0, SCREEN_HEIGHT - GROUND_HEIGHT, 20, _.gapMid)),
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

      ctx.fillStyle = "lime"
      for ((blockX, gapY) <- state.blocks) {
        ctx.fillRect(blockX, 0, BLOCK_WIDTH, gapY)
        ctx.fillRect(blockX, gapY + GAP_HEIGHT, BLOCK_WIDTH, SCREEN_HEIGHT - (gapY + GAP_HEIGHT))
      }

      ctx.fillStyle = "brown"
      ctx.fillRect(0, SCREEN_HEIGHT - GROUND_HEIGHT, SCREEN_WIDTH, GROUND_HEIGHT)

      ctx.fillStyle = "red"
      ctx.fillRect(state.x, state.y, BIRD_WIDTH, BIRD_HEIGHT)
    }

    ctx.restore()
  }

  override protected def modelPerformance(model: Model[FlappyBirdAgent]): Double = 0

}
