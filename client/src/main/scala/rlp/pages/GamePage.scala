package rlp.pages

import com.thoughtworks.binding.Binding.{SingleMountPoint, Var, Vars}
import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.{Event, html, window}
import org.scalajs.dom.html.{Canvas, Div}
import org.scalajs.dom.raw.CanvasRenderingContext2D
import rlp.environment.Environment
import rlp._
import rlp.models.Model
import rlp.utils.{BackgroundProcess, KeyboardHandler}

import scala.scalajs.js.timers

abstract class GamePage[S, A] extends Page {

  protected val modelBuilders: List[Model.Builder[A]]

  protected def createEnvironment(model: Model[A]): Environment[S]
  protected def render(ctx: CanvasRenderingContext2D): Unit

  protected val performanceEntryGap: Int = 100
  protected def modelPerformance(model: Model[A]): Double

  protected val renderTraining: Var[Boolean] = Var(true)

  protected val models: Vars[Model[A]] = Vars()
  private var model: Model[A] = _

  lazy val modelBuilder = new ModelBuilder(modelBuilders, models)
  lazy val modelTrainer = new ModelTrainer(models, trainStep)
  lazy val modelComparison = new ModelComparison(models, performanceEntryGap)

  protected val aspectRatio: Double = 3.0/4
  protected val targetGameWidth = 800

  private var canvas: Canvas = _
  private var ctx: CanvasRenderingContext2D = _
  protected val keyboardHandler = new KeyboardHandler()

  protected val MAX_EPISODE_LENGTH = 1000
  private var episodeLength = 0
  protected var trainingEnvironment: Environment[S] = _

  private val renderProcess = new BackgroundProcess(() => render(ctx), "Rendering")

  override def start(): Unit = {
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

        // Asynchronously perform performance check
        if (model.gamesPlayed.get % performanceEntryGap == 0) {
          timers.setTimeout(20) {
            model.performanceHistory.get += modelPerformance(model)
          }
        }

        model.gamesPlayed := model.gamesPlayed.get + 1
      }
      trainingEnvironment.reset()
      episodeLength = 0
    }
  }

  @dom
  override lazy val content: Binding[Div] = {

    <div class="row page-container">

      <div class="col s12">
        <h5 class="center-align">{name}</h5>
        <p class="center-align">{description}</p>
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
            { modelTrainer.content.bind }
          </div>

          <div class="card-reveal">
            { modelBuilder.content.bind }
          </div>
        </div>
      </div>

      <div class="col s12">
        { modelComparison.content.bind }
      </div>

      {
        modelTrainer.selectedModel.bind match {
          case Some(newModel) => {
            this.model = newModel
            trainingEnvironment = createEnvironment(model)
          }
          case None => /* Do nothing */
        }
        ""
      }
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