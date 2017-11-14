package rlp.pages

import com.thoughtworks.binding.Binding.{Constants, Var}
import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.Event
import org.scalajs.dom.html.{Canvas, Div}
import org.scalajs.dom.raw.{CanvasRenderingContext2D, HTMLElement}
import org.scalajs.dom.window.performance
import rlp.controllers.ModelController
import rlp.environment.{Agent, Environment}
import rlp._

import scala.scalajs.js.{Date, timers}

object GamePage {

  sealed trait TrainState
  object Stopped extends TrainState
  object Paused extends TrainState
  object Playing extends TrainState

  val GAME_SPEEDS = List(1, 2, 5, 10, -1)
  val GAME_SPEED_VALUES = GAME_SPEEDS.init.map("x"+_) ++ List("Max")
}

abstract class GamePage[A] {

  import GamePage._

  /* Abstract vars */
  protected val modelControllers: List[ModelController[A]]

  protected def initTraining(): Unit
  protected def trainStep(): Unit
  protected def render(ctx: CanvasRenderingContext2D): Unit

  protected val trainState: Var[TrainState] = Var(Stopped)
  protected val gameSpeed: Var[Int] = Var(0)
  protected val modelIdx: Var[Int] = Var(0)
  protected val renderTraining: Var[Boolean] = Var(true)

  private var ctx: CanvasRenderingContext2D = _

  /* Timers */
  private var trainingTimer: timers.SetIntervalHandle = _
  private var renderTimer: timers.SetIntervalHandle = _

  def start(): Unit = {
    renderTimer = timers.setInterval(1000 * Environment.DELTA) { render(ctx) }
  }

  private def createTrainingTimer(): timers.SetIntervalHandle = {
    val speed = GAME_SPEEDS(gameSpeed.get)

    if (speed > 0) {
      Logger.log(s"Began training at ${speed / Environment.DELTA}fps")
      timers.setInterval(1000 * Environment.DELTA / speed) { trainStep() }
    } else {
      val trainTime = benchmarkTraining()
      val targetTimeStep = 7.0
      val runsPerTimeStep = ((targetTimeStep-1)  / trainTime).toInt

      Logger.log(s"Ran training time benchmark - ${trainTime}ms")
      Logger.log(s"Began training at ${1000.0 * runsPerTimeStep / targetTimeStep}fps")

      timers.setInterval(targetTimeStep) {
        for (_ <- 0 until runsPerTimeStep) trainStep()
      }
    }
  }

  private def benchmarkTraining(): Double = {
    val numRuns = 1000
    val startTime = performance.now()
    for (_ <- 0 until numRuns) trainStep()
    (performance.now() - startTime) / numRuns
  }

  protected def playClicked(): Unit = {
    val wasStopped = trainState.get == Stopped
    trainState := Playing
    gameSpeed := 1

    if (wasStopped) initTraining()
    trainingTimer = createTrainingTimer()
  }

  protected def stopClicked(): Unit = {
    trainState := Stopped

    if (trainingTimer != null) timers.clearInterval(trainingTimer)
  }

  protected def pauseClicked(): Unit = {
    trainState := Paused
    timers.clearInterval(trainingTimer)
  }

  protected def fastForwardClicked(): Unit = {
    gameSpeed := (gameSpeed.get + 1) % GAME_SPEEDS.length
    timers.clearInterval(trainingTimer)
    trainingTimer = createTrainingTimer()
  }

  protected def modelChanged(): Unit = {

  }

  protected def toggleRenderTraining(): Unit = {
    renderTraining := !renderTraining.get
  }

  @dom
  final lazy val content: Binding[Div] = {

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

        <div class="col s9">
          { gameContainer.bind }
        </div>
        <div class="col s3 card-panel">
          { controlsSection.bind }
        </div>

        { taskBar.bind }

        <div class="col s12 card-panel">
          { modelControllers(modelIdx.bind).modelOptions.bind }
        </div>

        <div class="col s12 card-panel">
          <h5 class="center-align">TODO: Graphs &amp; Statistics</h5>
        </div>

      </div>
    </div>
  }

  @dom
  protected lazy val taskBar: Binding[Div] = {

    <div class="col s6 offset-s3 card-panel">
      { trainingButtons.bind } <br />

      <h6>
        {
          if (trainState.bind == Playing) {
            s"Speed: ${GAME_SPEED_VALUES(gameSpeed.bind)}"
          } else {
            "Stopped"
          }
        }
      </h6> <br />

      { modelSelection.bind }
    </div>
  }

  @dom
  protected lazy val controlsSection: Binding[Div] = {
    <div>
      <h5>Game Controls</h5>
      <div class="switch">
        <label>
          Render Training?
          <input type="checkbox" checked={renderTraining.bind}
            onchange={ _:Event => toggleRenderTraining() }
          />
          <span class="lever"></span>
        </label>
      </div>

      { gameOptions.bind }
    </div>
  }

  @dom
  protected lazy val gameOptions: Binding[Div] = {
    <div>
      Empty!
    </div>
  }

  @dom
  protected lazy val trainingButtons: Binding[Div] = {
    val buttonStyle = "btn-floating waves-effect waves-circle "
    val state = trainState.bind

    <div class="valign-wrapper">
      <a class= {
         buttonStyle + "btn-medium orange right " +
           (if (state == Stopped) "disabled" else "")
         }
         onclick={ _:Event => stopClicked() }
      >
        <i class="material-icons">stop</i>
      </a>

      <a class={buttonStyle + "btn-large red"}
         onclick = { _:Event => if (state == Playing) pauseClicked() else playClicked() }
      >
        <i class="material-icons">
          { if (state == Playing) "pause" else "play_arrow" }
        </i>
      </a>

      <a class= {
         buttonStyle + "btn-medium orange left " +
           (if (state != Playing) "disabled" else "")
         }
         onclick={ _:Event => fastForwardClicked() }
      >
        <i class="material-icons">fast_forward</i>
      </a>
    </div>
  }

  @dom
  protected lazy val modelSelection: Binding[Div] = {
    <div class="input-field">
      <select onchange={_:Event => modelChanged()} disabled={trainState.bind != Stopped}>
        {
          for ((model, idx) <- Constants(modelControllers.zipWithIndex: _*)) yield {
            <option value={idx.toString} selected={idx == modelIdx.bind}
              >{model.name}</option>
          }
        }
      </select>
      <label>Model Select</label>
    </div>
  }

  @dom
  protected lazy val gameContainer: Binding[Div] = {
    val canvas: Canvas = <canvas width={800} height={600}></canvas>
    ctx = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]

    <div class="card-panel">
      <h5 class="center-align">Game Container</h5>
      { canvas }
    </div>
  }
}
