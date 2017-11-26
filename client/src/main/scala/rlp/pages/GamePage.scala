package rlp.pages

import com.thoughtworks.binding.Binding.{BindingSeq, Constant, Constants, Var, Vars}
import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.{Event, document, html, window}
import org.scalajs.dom.html.{Canvas, Div}
import org.scalajs.dom.raw.{CanvasRenderingContext2D, HTMLElement}
import rlp.controllers.ModelController
import rlp.environment.Environment
import rlp._

import scala.scalajs.js

abstract class GamePage[A] {

  protected val targetGameWidth = 800

  protected val gameSpeedMultiplier = List(1, 2, 5, 10, -1)
  protected val gameSpeedToString = gameSpeedMultiplier.init.map("x"+_) ++ List("Max")

  /* Abstract vars */
  protected val modelControllerBuilders: List[ModelController.Builder[A]]

  protected val aspectRatio: Double = 3.0/4

  protected def initModel(model: Model[A]): Unit
  protected def trainStep(): Unit
  protected def render(ctx: CanvasRenderingContext2D): Unit

  protected val isTraining: Var[Boolean] = Var(false)
  protected val gameSpeed: Var[Int] = Var(0)
  protected val renderTraining: Var[Boolean] = Var(true)

  protected val models: Vars[Model[A]] = Vars()

  val modelSelect = new SelectHandler(
    "Model Select",
    models.map(m => m.controller.name + " - " + m.name),
    Constant(false)
  )

  val modelExists = Binding { models.bind.nonEmpty }

  val selectedModel: Binding[Option[Model[A]]] = Binding {
    if (modelExists.bind) {
      val model = models.bind(modelSelect.selectedIndex.bind)
      modelChanged(model)

      Some(model)
    } else {
      None
    }
  }

  lazy val modelBuilder = new ModelBuilder(modelControllerBuilders, models)

  private var canvas: Canvas = _
  private var ctx: CanvasRenderingContext2D = _
  protected val keyboardHandler = new KeyboardHandler()

  private val trainingProcess = new BackgroundProcess(trainStep)
  private val renderProcess = new BackgroundProcess(() => render(ctx))

  def start(): Unit = {
    renderProcess.start(Environment.FPS)
    window.onresize = { _:Event => pageResized() }
    pageResized()
  }

  protected def modelChanged(model: Model[A]): Unit = {
    if (isTraining.get) pauseTraining()
    initModel(model)

    js.timers.setTimeout(100) { js.Dynamic.global.Materialize.updateTextFields() }
  }

  @dom
  protected def startTraining(): Unit = {
    val model = selectedModel.bind.get
    model.gamesPlayed := 0

    trainingProcess.start(Environment.FPS * gameSpeedMultiplier(gameSpeed.get))
    isTraining := true
  }

  protected def pauseTraining(): Unit = {
    trainingProcess.stop()
    isTraining := false
  }

  @dom
  protected def resetTraining(): Unit = {
    val model = selectedModel.bind.get

    model.controller.resetAgent()
    model.gamesPlayed := 0
  }

  protected def fastForwardTraining(): Unit = {
    gameSpeed := (gameSpeed.get + 1) % gameSpeedMultiplier.length
    trainingProcess.stop()
    trainingProcess.start(Environment.FPS * gameSpeedMultiplier(gameSpeed.get))
  }

  protected def toggleRenderTraining(): Unit = {
    renderTraining := !renderTraining.get
  }

  protected def pageResized(): Unit = {
    val container = document.getElementById("canvas-container").asInstanceOf[Div]
    val width = Math.min(targetGameWidth, container.offsetWidth - 50)

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
              { modelBuilder.content.bind }
            </div>
          </div>
        </div>

      </div>

    </div>
  }

  @dom
  protected lazy val modelViewSection: Binding[Div] = {

    <div class="row">

      <div class="row grey col s12 lighten-3" id="model-training">
        <div class="col s3">
          <span class="card-title">Model Training</span>
        </div>

        <div class="col s3">
          { modelSelect.handler.bind }
        </div>

        <div class="col s6" id="model-training-btns">
          {
            val btnStyle = "btn waves-effect waves-light"

            <!-- TODO: IMPLEMENT FUNCTIONALITY FOR THESE  -->
            <a class={btnStyle + " disabled"}>Clone</a>
            <a class={btnStyle + " disabled"}>Duplicate</a>
            <a class={btnStyle + " activator"}>New</a>
          }
          <!--<a class="btn-floating btn waves-effect waves-light red activator right"><i class="material-icons">add</i></a>-->
        </div>
      </div>

      <div class="col s12">
        { if (modelExists.bind) trainingControls.bind else <!-- --> }
      </div>

      <div class="col s12">
        {
          selectedModel.bind match {
            case None => <!-- -->
            case Some(model) => model.controller.modelViewer.bind
          }
        }
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
    <div class="row">
      <div class="col s6">
        { trainingButtons.bind }
      </div>

      <div class="col s6 valign-wrapper">
        <h6 class="center-align">
          {
          selectedModel.bind match {
            case Some(model) => s"Games Played: ${model.gamesPlayed.bind}"
            case None => ""
          }
          }
        </h6>
      </div>
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
    val training = isTraining.bind

    <div class="center-align" id="buttons-container">
      <div class="valign-wrapper">
        <a class= {buttonStyle + "btn-medium orange"}
           onclick={ _:Event => resetTraining() }
        >
          <i class="material-icons">replay</i>
        </a>

        <a class={buttonStyle + "btn-large red"}
           onclick = { _:Event => if (training) pauseTraining() else startTraining() }
        >
          <i class="material-icons">
            { if (training) "pause" else "play_arrow" }
          </i>
        </a>

        <a class= {
             buttonStyle + "btn-medium orange " + (if (training) "" else "disabled")
           }
           onclick={ _:Event => fastForwardTraining() }
        >
          <i class="material-icons">fast_forward</i>
        </a>
        <span id="training-speed"> { if (isTraining.bind) gameSpeedToString(gameSpeed.bind) else "" } </span>
      </div>
    </div>
  }

  @dom
  protected lazy val gameContainer: Binding[Div] = {
    canvas = <canvas class="center-align" width={targetGameWidth} height={(targetGameWidth * aspectRatio).toInt}></canvas>
    ctx = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]

    <div id="canvas-container" class="center-align valign-wrapper"> { canvas } </div>
  }
}