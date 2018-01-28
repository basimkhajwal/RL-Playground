package rlp.pages

import com.thoughtworks.binding.Binding.{Constants, SingleMountPoint, Var, Vars}
import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.{Event, html, window}
import org.scalajs.dom.html.{Canvas, Div}
import org.scalajs.dom.raw.CanvasRenderingContext2D
import rlp.environment.Environment
import rlp._
import rlp.models.Model
import rlp.utils.{BackgroundProcess, KeyboardHandler, Logger}
import rlp.views.{ModelBuilder, ModelComparison, ModelTrainer}

import scala.concurrent.ExecutionContext.Implicits.global
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

  lazy val modelBuilder = new ModelBuilder(modelBuilders, models, modelDAO)
  lazy val modelTrainer = new ModelTrainer(models, modelBuilders, modelDAO, modelPerformance, trainStep)
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

  val gameDescription: String
  val inputDescription: String
  val actionDescription: String
  val rewardDescription: String

  private var initialised: Boolean = false

  override def start(): Unit = {
    renderProcess.start(Environment.FPS)
    window.onresize = { _:Event => pageResized() }
    pageResized()

    val game = getElem[Div]("game-row")
    game.tabIndex = 0
    keyboardHandler.register(game)

    if (!initialised) {
      initialised = true

      modelDAO.getAll() map { modelStores =>

        Logger.log("GamePage", s"Loading ${modelStores.length} model stores from database")

        for {
          store <- modelStores
          if store.environmentName == name
        } {
          try {

            modelBuilders.find(_._1 == store.agentName) match {
              case Some((_, builder)) => {
                val model = builder()
                model.load(store)
                models.get += model
              }

              case None =>
            }

          } catch {
            case e: Exception =>
              Logger.log("GamePage", s"Error loading modelStore ${store.id} - " + e.getMessage)
          }
        }

      } recover {
        case error:Throwable => Logger.log("GamePage", "DB access error - " + error.getMessage)
      }
    }

  }

  override def stop(): Unit = {
    renderProcess.stop()
    if (modelTrainer.isTraining.get) modelTrainer.pauseTraining()
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
            model.logPerformance(modelPerformance(model))
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
        <div class="description card-panel">
          <span class="flow-text">{gameDescription}</span>
          <br />
          <br />
          {
            val descriptions = List(
              "Input" -> inputDescription,
              "Actions" -> actionDescription,
              "Rewards" -> rewardDescription
            )

            for ((name, desc) <- Constants(descriptions :_*)) yield {
              <div class="description-item row">
                <h6 class="col s1 offset-s1"><strong>{name}</strong></h6>
                <h6 class="col s7">{desc}</h6>
              </div>
            }
          }
        </div>
      </div>

      <div class="col s12">
        <div class="card">
          <div class="row vertical-stretch-row" id="game-row">
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

          { modelBuilder.content.bind }
        </div>
      </div>

      <div class="col s12">
        { modelComparison.content.bind }
      </div>

      {
        modelTrainer.selectedModel.bind match {
          case Some(newModel : Model[A]) => {
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
      <div class="center-align">
        <span class="card-title">Game Options</span>
      </div>
      <br />
      <br />

      <div class="switch center-align">
        <label>
          Render Training
          <input type="checkbox" checked={!renderTraining.bind} onchange={ _:Event => toggleRenderTraining() } />
          <span class="lever"></span>
          Play Game
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