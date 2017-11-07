package rlp.pages


import com.thoughtworks.binding.Binding.Var
import rlp._
import rlp.environment.{Environment, NaivePongAgent, Pong}
import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.{CanvasRenderingContext2D, document, html}
import org.scalajs.dom.html.{Canvas, Div}
import org.scalajs.dom.raw.Event
import rlp.ai.QTable

import scala.scalajs.js.{Date, timers}

class PongPage {

  import Pong._

  class LearningAgent(val table: QTable) extends PongAgent {

    val xSamples = 20
    val ySamples = 10
    val dirSamples = 1

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

  sealed trait PlayState
  object Stopped extends PlayState
  object Paused extends PlayState
  object Playing extends PlayState

  private val playState: Var[PlayState] = Var(Stopped)
  private val gameSpeed = Var(0)

  def stopClicked(): Unit = {
    playState := Stopped
  }

  def playClicked(): Unit = {
    playState := Playing
    gameSpeed := 1
  }

  def pauseClicked(): Unit = {
    playState := Paused
  }

  def fastForwardClicked(): Unit = {
    gameSpeed := gameSpeed.get + 1
  }

  @dom
  def taskBar(): Binding[Div] = {
    val buttonStyle = "btn-floating waves-effect waves-circle "

    <div class="col s6 offset-s3 card-panel">
       {
        val currentState = playState.bind

        <div class="row valign-wrapper">
          <a class= {
              buttonStyle + "btn-medium orange right " +
              (if (currentState == Stopped) "disabled" else "")
             }
             onclick={ _:Event => stopClicked() }
          >
            <i class="material-icons">stop</i>
          </a>

          <a class={buttonStyle + "btn-large red"}
             onclick = { _:Event => if (currentState == Playing) pauseClicked() else playClicked() }
          >
            <i class="material-icons">
              { if (currentState == Playing) "pause" else "play_arrow" }
            </i>
          </a>

          <a class= {
               buttonStyle + "btn-medium orange left " +
               (if (currentState != Playing) "disabled" else "")
             }
             onclick={ _:Event => fastForwardClicked() }
          >
            <i class="material-icons">fast_forward</i>
          </a>
        </div>
      }
      <div class="row">
        <div>
          <input type="checkbox" checked={true} name="render" id="render"/>
          <label for="render">Render?</label>
        </div>
        <h6>Speed: x{gameSpeed.bind.toString}</h6>
        <h6>Games Played: {gamesPlayed.bind.toString}</h6>
      </div>
    </div>
  }

  @dom
  def content(): Binding[Div] = {

    val canvas: Canvas = <canvas width={800} height={600}></canvas>
    ctx = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]

    <div id="app">

      <nav class="orange z-depth-0">
        <div class="nav-wrapper container">
          <a href="#" class="brand-logo left">RL-Playground</a>
          <p id="subtitle" class="right hide-on-med-and-down">
            An interactive reinforcement learning demonstration
          </p>
        </div>
      </nav>

      <div class="row container">

        <div class="col s12">
          <h5 class="center-align">PONG</h5>
        </div>

        <div class="col s12">
          <div class="card-panel">
            <h5 class="center-align">Game Container</h5>
            { canvas }
          </div>
        </div>

        { taskBar().bind }

        <div class="col s12 card-panel">
          <h5 class="center-align">TODO: Model Selection</h5>
        </div>

        <div class="col s12 card-panel">
          <h5 class="center-align">TODO: Graphs &amp; Statistics</h5>
        </div>

      </div>
    </div>
  }
}
