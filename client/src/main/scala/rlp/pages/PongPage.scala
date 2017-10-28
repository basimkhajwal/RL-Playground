package rlp.pages

import javafx.scene.control.RadioButton

import com.thoughtworks.binding.Binding.Var
import rlp._
import rlp.environment.{Environment, NaivePongAgent, Pong}
import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.{CanvasRenderingContext2D, document, html}
import org.scalajs.dom.html.{Canvas, Div}
import org.scalajs.dom.raw.Event

import scala.scalajs.js.{Date, timers}

class PongPage {

  import Pong._

  class QTable(val dimensions: Array[Int], val numActions: Int) {

    val table = new Array[Double](dimensions.product * numActions)

    def convertToIndex(indexes: Array[Int], act: Int): Int = {
      var idx = 0
      var dimSize = 1
      for ((i, sz) <- indexes zip dimensions) {
        idx += i * dimSize
        dimSize *= sz
      }
      idx + act * dimSize
    }

    def apply(indexes: Array[Int], act: Int): Double = table(convertToIndex(indexes, act))
    def update(indexes: Array[Int], act: Int,  value: Double): Unit = {
      table(convertToIndex(indexes, act)) = value
    }
  }

  val xSamples = 20
  val ySamples = 10
  val dirSamples = 1

  class LearningAgent(val table: QTable) extends PongAgent {

    val alpha = 0.05
    val lambda = 0.99
    var epsilon = 0.1

    def this() {
      this(
        new QTable(
          Array(
            xSamples, ySamples,
            dirSamples,
            ySamples, //ySamples
          ), 2
        )
      )
    }

    def stateIndexes(state: AgentState) = Array(
      (xSamples * state.ballPos.x / SCREEN_WIDTH).toInt,
      (ySamples * state.ballPos.y / SCREEN_HEIGHT).toInt,
      (dirSamples * state.ballDir.angle() / (2 * Math.PI)).toInt,
      (ySamples * state.currentPaddle / SCREEN_HEIGHT).toInt,
      //(ySamples * state.otherPaddle / SCREEN_HEIGHT).toInt,
    )

    private var lastAction: Action = _

    override def act(state: AgentState): Action = {
      val idx = stateIndexes(state)

      val upVal = table(idx, 0)
      val downVal = table(idx, 1)
      // val upProb = Math.exp(upVal) / (Math.exp(upVal) + Math.exp(downVal))
      //lastAction = if (Math.random() < upProb) UpAction else DownAction

      lastAction =
        if (Math.random() < epsilon || Math.abs(upVal - downVal) < 0.01) {
          if (Math.random() > 0.5) UpAction else DownAction
        } else {
          if (upVal > downVal) UpAction else DownAction
        }

      lastAction
    }

    override def percept(prevState: AgentState, newState: AgentState, reward: Double): Unit = {
      val s1 = stateIndexes(prevState)
      val s2 = stateIndexes(newState)
      val a = if (lastAction == UpAction) 0 else 1

      table(s1, a) += alpha * (reward + lambda * Math.max(table(s2, 0), table(s2, 1)) - table(s1, a))

      epsilon *= 0.99
    }
  }

  private def renderState(ctx: CanvasRenderingContext2D, state: State): Unit = {
    ctx.save()
    ctx.scale(ctx.canvas.width / Pong.SCREEN_WIDTH, ctx.canvas.height / Pong.SCREEN_HEIGHT)

    ctx.fillStyle = "black"
    ctx.fillRect(0, 0, SCREEN_WIDTH, SCREEN_HEIGHT)

    ctx.fillStyle = "lime"
    ctx.fillRect(
      PADDLE_PADDING, state.leftPaddleY,
      PADDLE_WIDTH, PADDLE_HEIGHT
    )
    ctx.fillRect(
      SCREEN_WIDTH - PADDLE_PADDING - PADDLE_WIDTH, state.rightPaddleY,
      PADDLE_WIDTH, PADDLE_HEIGHT
    )
    ctx.fillRect(state.ballPos.x, state.ballPos.y, BALL_SIZE, BALL_SIZE)

    ctx.restore()
  }

  private val gamesPlayed = Var(0)

  private var ctx: CanvasRenderingContext2D = _
  private val agent1 = new LearningAgent()
  private val agent2 = new LearningAgent(agent1.table)
  private val environment = new Pong(agent1, agent2)

  private var renderTimer: timers.SetIntervalHandle = _
  private var lastTimer: timers.SetIntervalHandle = _

  private var stepCount: Int = 0
  private var msPerStep: Double = -1
  private var stepRate: Int = 1

  def start(refreshRate: Int = 30, render: Boolean = true): Unit = {

    if (msPerStep < 0) msPerStep = runBenchmark()

    if (lastTimer != null) {
      timers.clearInterval(lastTimer)
    }

    if (render && renderTimer == null) {
      renderTimer = timers.setInterval(1000.0 * Environment.DELTA) { renderTick() }
    } else if (!render && renderTimer != null) {
      timers.clearInterval(renderTimer)
      renderTimer = null
    }

    lastTimer = timers.setInterval(1000.0 / refreshRate) { updateTick() }
  }

  def runBenchmark(): Double = {
    val num = 10000
    val e = new Pong(new LearningAgent(), new LearningAgent())

    val t1 = Date.now()
    for (_ <- 0 until num) e.step()
    val t2 = Date.now()

    (t2 - t1) / num
  }

  def renderTick(): Unit = {
    renderState(ctx, environment.getState())
  }

  def updateTick(): Unit = {
    for (_ <- 0 until stepRate) {
      if (environment.step() || stepCount > 1000) {
        environment.reset()
        gamesPlayed := gamesPlayed.get + 1
        stepCount = 0
      }
      stepCount += 1
    }
  }

  def updateRefreshRate(): Boolean = {
    val refreshRate = document.querySelector("input[name=rate]:checked").asInstanceOf[html.Input].value.toInt
    val isRender = document.getElementById("render").asInstanceOf[html.Input].checked

    if (refreshRate == -1) {
      stepRate = (5 / msPerStep).toInt
      start(1000 / 6, isRender)
    } else {
      stepRate = 1
      start(refreshRate, isRender)
    }

    false
  }

  @dom
  def content(): Binding[Div] = {

    val canvas: Canvas = <canvas width={800} height={600}></canvas>
    ctx = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]

    <div id="app">
      <div class="row">

        <div class="col s12">
          <h5 class="white-text center-align">PONG</h5>
        </div>

        <div class="col s8">
          <div class="card-panel orange lighten">
            <h5 class="white-text center-align">Game Container</h5>
            { canvas }
          </div>
        </div>

        <div class="col s4">
          <div class="card-panel red lighten">
            <h5 class="white-text">Controls Container</h5>
            <form onsubmit={_:Event => false}>
              <h6>Updates Per Second</h6>
              <div>
                <input type="radio" id="r1" name="rate" value="30" checked={true} />
                <label for="r1">Normal (30 fps)</label>

                <input type="radio" id="r2" name="rate" value="100" />
                <label for="r2">Fast (100 fps)</label>

                <input type="radio" id="r3" name="rate" value="250" />
                <label for="r3">Faster (250 fps)</label>

                <input type="radio" id="r4" name="rate" value="-1" />
                <label for="r4">Fastest (browser dependent)</label>
              </div>
              <div>
                <input type="checkbox" checked={true} name="render" id="render"/>
                <label for="render">Render?</label>
              </div>
              <div>
                <button onclick={_:Event => updateRefreshRate() }>
                  Update Changes
                </button>
                <button onclick={_:Event => environment.reset() }>
                  New Game
                </button>
              </div>
            </form>
            <p>
              Games played { gamesPlayed.bind.toString }
            </p>
          </div>
        </div>
      </div>
    </div>
  }
}
