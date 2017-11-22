package rlp.pages

import com.thoughtworks.binding.Binding.{Constant, Constants, Var}
import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.{Event, document, html, window}
import org.scalajs.dom.html.{Canvas, Div}
import org.scalajs.dom.raw.{CanvasRenderingContext2D, HTMLElement}
import org.scalajs.dom.window.performance
import rlp.controllers.ModelController
import rlp.environment.{Agent, Environment}
import rlp._

import scala.collection.mutable
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
  protected val modelControllerBuilders: List[ModelController.Builder[A]]

  protected def initTraining(): Unit
  protected def trainStep(): Unit
  protected def render(ctx: CanvasRenderingContext2D): Unit

  protected val trainState: Var[TrainState] = Var(Stopped)
  protected val gameSpeed: Var[Int] = Var(0)
  protected val renderTraining: Var[Boolean] = Var(true)
  protected val gameCount: Var[Int] = Var(0)

  protected val models: mutable.Map[String, ModelController[A]] = mutable.Map()
  protected val selectedModelBinding: Var[Option[ModelController[A]]] = Var(None)

  private var canvas: Canvas = _
  private var ctx: CanvasRenderingContext2D = _
  protected val keyboardHandler = new KeyboardHandler()

  /* Timers */
  private var trainingTimer: timers.SetIntervalHandle = _
  private var renderTimer: timers.SetIntervalHandle = _

  def start(): Unit = {
    renderTimer = timers.setInterval(1000 * Environment.DELTA) { render(ctx) }
    window.onresize = { _:Event => pageResized() }
    pageResized()
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
    if (trainState.get == Stopped) {
      gameCount := 0
      initTraining()
    } else {
      gameSpeed := 0
    }

    trainingTimer = createTrainingTimer()
    trainState := Playing
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

  protected def toggleRenderTraining(): Unit = {
    renderTraining := !renderTraining.get
  }

  protected def pageResized(): Unit = {
    val container = document.getElementById("canvas-container").asInstanceOf[Div]
    val width = Math.min(800, container.offsetWidth - 50)
    val aspectRatio = (canvas.height * 1.0) / canvas.width

    canvas.width = width.toInt
    canvas.height = (aspectRatio * width).toInt
  }

  @dom
  final lazy val content: Binding[Div] = {

    <div id="app">

      <nav class="teal z-depth-0">
        <div class="nav-wrapper page-container">
          <a href="#" class="brand-logo left">RL-Playground</a>
          <p id="subtitle" class="right hide-on-med-and-down">
            An interactive reinforcement learning demonstration
          </p>
        </div>
      </nav>

      <div class="row page-container">

        <div class="col s12">
          <h5 class="center-align">Pong</h5>
        </div>

        <div class="col s12">
          <div class="card">
            <div class="row" id="game-row">
              <div class="col s8">
                { gameContainer.bind }
              </div>
              <div class="col s4 teal lighten-5">
                { controlsSection.bind }
              </div>
            </div>
          </div>
        </div>

        <div class="col s12">
          <div class="card" id="model-select">
            <div class="card-content">
              { modelViewSection.bind }
            </div>

            <div class="card-reveal">
              { modelBuildSection.bind }
            </div>
          </div>
        </div>

        <div class="col s12">
          <div class="card-panel">
            <h5 class="center-align">TODO: Graphs &amp; Statistics</h5>
          </div>
        </div>

      </div>

    </div>
  }

  @dom
  protected lazy val modelBuildSection: Binding[Div] = {

    def initialName(idx: Int): String = {
      if (!models.contains("Model"+idx)) "Model"+idx else initialName(idx+1)
    }

    val newModelControllerSelect = new SelectHandler("Model Type", modelControllerBuilders.map(_._1), Constant(false))
    val newModelController = Binding {
      modelControllerBuilders(newModelControllerSelect.selectedIndex.bind)._2()
    }
    val valid: Var[Boolean] = Var(true)
    val modelName: Var[String] = Var(initialName(1))

    def onNameChange(): Unit = {
      val modelNameElem = getElem[html.Input]("model-name")

      if (models.contains(modelNameElem.value)) {
        modelNameElem.setCustomValidity("Invalid")
      } else {
        modelNameElem.setCustomValidity("")
        modelName := modelNameElem.value
      }
    }

    def onCreate(): Unit = {

    }

    def onClose(): Unit = {
      getElem[html.Span]("close-button").click()
    }

    <div class="row">

      <div class="col s3">
        <span class="card-title">Model</span>
      </div>

      <div class="col s5">
        <span class="card-title center-align">Create New</span>
      </div>

      <div class="col s3">
        <span class="card-title right" id="close-button"><i class="material-icons">close</i></span>
      </div>

      <div class="col s3 offset-s2">
        { newModelControllerSelect.handler.bind }
      </div>

      <div class="col s3 offset-s2 input-field">
        <input id="model-name" class="validate" type="text" value={modelName.bind} onchange={_:Event => onNameChange()}/>
        <label for="model-name" data:data-error="Model name already exists">Model Name</label>
        <!-- Add some sort of automated validation for model name -->
      </div>

      <div class="col s12">
        { newModelController.bind.modelBuilder.bind }
      </div>

      <div class="col s2 offset-s4">
        <a class="waves-effect waves-light btn" onclick={_:Event => onClose()}>Cancel</a>
      </div>

      <div class="col s2">
        <a class="waves-effect waves-light btn" onclick={_:Event => onCreate()}>Create</a>
        <!-- TODO: Only enable when valid -->
      </div>

    </div>
  }

  @dom
  protected lazy val modelViewSection: Binding[Div] = {
    <div class="row">
      <div class="col s3">
        <span class="card-title">Model</span>
      </div>

      <div class="col s5">
        <!-- TODO: Add some model select code here -->
      </div>

      <div class="col s3">
        <a class="btn-floating btn waves-effect waves-light red activator right"><i class="material-icons">add</i></a>
      </div>

      <div class="col s12">
        <!-- TODO: Only enable when a model is bound -->
        { trainingControls.bind }
      </div>

      <div class="col s12">
        <!-- TODO: Bind into the bound model's model viewer stuff -->
      </div>

    </div>
  }

  @dom
  protected lazy val controlsSection: Binding[Div] = {
    <div id="control-section">
      <br />
      <span class="card-title">Game Options</span>
      <br />

      <div class="switch center-align">
        <label>
          Play Game
          <input type="checkbox" checked={renderTraining.bind} onchange={ _:Event => toggleRenderTraining() } />
          <span class="lever"></span>
          Render Training
        </label>
      </div>

      { gameOptions.bind }
    </div>
  }

  @dom
  protected lazy val trainingControls: Binding[Div] = {
    <div>
      { trainingButtons.bind } <br />

      <div class="row">
        <div class="col s6">
          <h6 class="center-align">
            {
            if (trainState.bind == Playing) s"Training Speed: ${GAME_SPEED_VALUES(gameSpeed.bind)}"
            else if (trainState.bind == Paused) "Training Paused"
            else "Training Stopped"
            }
          </h6>
        </div>
        <div class="col s6">
          <h6 class="center-align">
            { s"Games Played: ${gameCount.bind.toString}" }
          </h6>
        </div>
      </div>
      <br />
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
    val buttonStyle = "center-align btn-floating waves-effect waves-circle "
    val state = trainState.bind

    <div class="center-align" id="buttons-container">
      <div class="valign-wrapper">
        <a class= {
           buttonStyle + "btn-medium orange " +
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
           buttonStyle + "btn-medium orange " +
             (if (state != Playing) "disabled" else "")
           }
           onclick={ _:Event => fastForwardClicked() }
        >
          <i class="material-icons">fast_forward</i>
        </a>
      </div>
    </div>
  }

  @dom
  protected lazy val gameContainer: Binding[Div] = {
    canvas = <canvas class="center-align" width={800} height={600}></canvas>
    ctx = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]

    <div id="canvas-container" class="center-align valign-wrapper"> { canvas } </div>
  }
}
