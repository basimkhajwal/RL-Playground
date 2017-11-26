package rlp.pages

import com.thoughtworks.binding.Binding.{Var, Vars}
import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.{Event, window}
import org.scalajs.dom.html.{Canvas, Div}
import org.scalajs.dom.raw.{CanvasRenderingContext2D}
import rlp.controllers.ModelController
import rlp.environment.Environment
import rlp._

abstract class GamePage[S, A] {

  protected val targetGameWidth = 800

  /* Abstract vars */
  protected val modelControllerBuilders: List[ModelController.Builder[A]]

  protected val aspectRatio: Double = 3.0/4

  protected val MAX_EPISODE_LENGTH = 1000

  protected def initModel(model: Model[A]): Environment[S]
  protected def render(ctx: CanvasRenderingContext2D): Unit

  protected val renderTraining: Var[Boolean] = Var(true)

  protected val models: Vars[Model[A]] = Vars()
  private var model: Model[A] = _

  lazy val modelBuilder = new ModelBuilder(modelControllerBuilders, models)
  lazy val modelTrainer = new ModelTrainer(models, modelBuilder, trainStep)

  private var canvas: Canvas = _
  private var ctx: CanvasRenderingContext2D = _
  protected val keyboardHandler = new KeyboardHandler()

  private val renderProcess = new BackgroundProcess(() => render(ctx), "Rendering")

  private var episodeLength = 0
  protected var trainingEnvironment: Environment[S] = _

  def start(): Unit = {
    renderProcess.start(Environment.FPS)
    window.onresize = { _:Event => pageResized() }
    pageResized()

    val game = getElem[Div]("game-row")
    game.tabIndex = 0
    keyboardHandler.register(game)
  }

  protected def toggleRenderTraining(): Unit = {
    renderTraining := !renderTraining.get
  }

  protected def pageResized(): Unit = {
    val container = getElem[Div]("canvas-container")
    val width = Math.min(targetGameWidth, container.offsetWidth - 50)

    canvas.width = width.toInt
    canvas.height = (aspectRatio * width).toInt
  }

  protected def trainStep(): Unit = {
    episodeLength += 1
    if (trainingEnvironment.step() || episodeLength > MAX_EPISODE_LENGTH) {
      if (episodeLength <= MAX_EPISODE_LENGTH) {
        model.gamesPlayed := model.gamesPlayed.get + 1
      }
      trainingEnvironment.reset()
      episodeLength = 0
    }
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
              {
                modelTrainer.selectedModel.bind match {
                  case Some(model) => {
                    this.model = model
                    trainingEnvironment = initModel(model)
                  }
                  case None => /* Do nothing */
                }
                modelTrainer.content.bind
              }
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
  protected lazy val controlsSection: Binding[Div] = {
    <div id="control-section">
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
  protected lazy val gameOptions: Binding[Div] = {
    <div>Empty!</div>
  }

  @dom
  protected lazy val gameContainer: Binding[Div] = {
    canvas = <canvas class="center-align" width={targetGameWidth} height={(targetGameWidth * aspectRatio).toInt}></canvas>
    ctx = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]

    <div id="canvas-container" class="center-align valign-wrapper"> { canvas } </div>
  }
}