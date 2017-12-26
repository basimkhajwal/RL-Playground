package rlp.environment

import rlp.agent.Agent

import scala.collection.mutable.ListBuffer
import scala.util.Random

object FlappyBird {

  val SCREEN_WIDTH = 800.0
  val SCREEN_HEIGHT = 600.0

  val GROUND_HEIGHT = 100.0

  val BIRD_WIDTH = 50.0
  val BIRD_HEIGHT = 50.0
  val BIRD_SPEED = 50.0
  val BIRD_JUMP_SPEED = 100.0

  val GRAVITY = 20.0

  val GAP_HEIGHT = 100.0
  val GAP_PADDING = 50.0
  val INITIAL_BLOCK_POS = 500.0
  val BLOCK_SPACING = 300.0
  val BLOCK_WIDTH = 60.0

  case class State(
    x: Double,
    y: Double,
    vy: Double,
    blocks: List[(Double, Double)]
  )

  sealed trait Action
  case object NoAction extends Action
  case object JumpAction extends Action

  case class AgentState(
    y: Double,
    vy: Double,
    blockDist: Double,
    gapY: Double
  )
}

class FlappyBird(val agent: Agent[FlappyBird.AgentState, FlappyBird.Action]) extends Environment[FlappyBird.State] {

  import FlappyBird._

  private val random = new Random()

  private var x: Double = _
  private var y: Double = _
  private var vy: Double = _
  private var blocks = ListBuffer.empty[(Double, Double)]

  override def reset(seed: Int): Unit = {
    random.setSeed(seed)

    x = 0
    y = (SCREEN_HEIGHT - GROUND_HEIGHT - BIRD_HEIGHT) / 2
    vy = 0

    blocks.clear()
    blocks += ((INITIAL_BLOCK_POS, generateGapY()))
  }

  override def setState(state: State): Unit = {
    x = state.x
    y = state.y
    vy = state.vy
    blocks.clear()
    blocks.appendAll(state.blocks)
  }

  override def getState(): State = State(x, y, vy, blocks.toList)

  private def generateGapY(): Double = {
    GAP_PADDING + math.random() * (SCREEN_HEIGHT - GROUND_HEIGHT - 2 * GAP_PADDING - GAP_HEIGHT)
  }

  private def getAgentState(): AgentState = {
    
    var blockDist = SCREEN_WIDTH
    var gapHeight = 0.0
    for ((blockX, gapY) <- blocks) {
      if (blockX >= x && (blockX - x) < blockDist) {
        blockDist = blockX - x
        gapHeight = gapY
      }
    }

    AgentState(y, vy, blockDist, gapHeight)
  }

  override def step(): Boolean = {

    agent.act(getAgentState()) match {
      case NoAction =>
      case JumpAction => {
        vy = -BIRD_JUMP_SPEED
      }
    }

    vy += GRAVITY * Environment.DELTA
    y += vy * Environment.DELTA
    x += BIRD_SPEED * Environment.DELTA

    val furthestBlock = blocks.map(_._1).max
    if (furthestBlock < x + SCREEN_WIDTH) {
      blocks += (
        (furthestBlock + BLOCK_SPACING + BLOCK_WIDTH, generateGapY())
      )
    }

    blocks = blocks.filter(_._1 > x - SCREEN_WIDTH)

    val groundCollision = y + BIRD_HEIGHT >= SCREEN_HEIGHT - GROUND_HEIGHT

    val blockCollision = blocks.exists { case (blockX, gapY) =>
      val withinX = x + BIRD_WIDTH >= blockX && x <= blockX + BLOCK_WIDTH
      val topHit = y < gapY
      val bottomHit = y + BIRD_HEIGHT > gapY + GAP_HEIGHT

      withinX && (topHit || bottomHit)
    }

    groundCollision || blockCollision
  }
}
