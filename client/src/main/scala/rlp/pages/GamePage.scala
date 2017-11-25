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

object GamePage {

  val GAME_SPEEDS = List(1, 2, 5, 10, -1)
  val GAME_SPEED_VALUES = GAME_SPEEDS.init.map("x"+_) ++ List("Max")
}

abstract class GamePage[A] {

  import GamePage._

  /* Abstract vars */
  protected val modelControllerBuilders: List[ModelController.Builder[A]]

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

    trainingProcess.start(Environment.FPS * GAME_SPEEDS(gameSpeed.get))
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
    gameSpeed := (gameSpeed.get + 1) % GAME_SPEEDS.length
    trainingProcess.stop()
    trainingProcess.start(Environment.FPS * GAME_SPEEDS(gameSpeed.get))
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

      </div>

    </div>
  }

  @dom
  protected lazy val modelBuildSection: Binding[Div] = {

    def findUnusedName(): String = {
      val names = models.get.map(_.name)
      var idx = 1
      while (names contains ("Model"+idx)) idx += 1
      "Model"+idx
    }

    val newModelSelect = new SelectHandler("Model Type", modelControllerBuilders.map(_._1), Constant(false))

    val controllerCache: Vars[(Int, ModelController[A])] = Vars()

    val modelController = Binding {
      val idx = newModelSelect.selectedIndex.bind
      val builder = modelControllerBuilders(idx)._2
      val cache = controllerCache.bind

      cache.find(c => c._1 == idx) match {
        case Some((_, controller)) => controller
        case None => {
          val controller = builder()
          controllerCache.get += ((idx, controller))
          controller
        }
      }
    }

    val modelName: Var[String] = Var(findUnusedName())

    val validName: Var[Boolean] = Var(true)
    val valid: Binding[Boolean] = Binding {
      validName.bind && modelController.bind.buildValid.bind
    }

    @dom
    def onNameChange(): Unit = {
      val modelNames = models.bind.map(_.name)
      val modelNameElem = getElem[html.Input]("model-name")

      if (modelNames contains modelNameElem.value) {
        modelNameElem.setCustomValidity("Invalid")
        validName := false
      } else {
        modelNameElem.setCustomValidity("")
        modelName := modelNameElem.value
        validName := true
      }
    }

    @dom
    def onCreate(): Unit = {
      modelController.bind.agent // Call build model
      models.get += Model(modelName.get, modelController.bind)

      onClose()
    }

    def onClose(): Unit = {
      // Reset builder
      controllerCache.get.clear()
      newModelSelect.selectedIndex := 0
      modelName := findUnusedName()
      validName := true

      getElem[html.Span]("close-button").click()
    }

    <div class="row">

      <div class="col s5 offset-s3">
        <span class="card-title center-align">Create New</span>
      </div>

      <div class="col s4">
        <span class="card-title right" id="close-button"><i class="material-icons">close</i></span>
      </div>

      <div class="col s3 offset-s2">
        { newModelSelect.handler.bind }
      </div>

      <div class="col s3 offset-s2 input-field">
        <input id="model-name" class="validate" type="text"
          value={modelName.bind} onchange={_:Event => onNameChange()} required={true}/>
        <label for="model-name" data:data-error="Model name empty or already exists">Model Name</label>
      </div>

      <div class="col s12">
        { modelController.bind.modelBuilder.bind }
      </div>

      <div class="col s2 offset-s4">
        <a class="waves-effect waves-light btn" onclick={_:Event => onClose()}>Cancel</a>
      </div>

      <div class="col s2">
        <a class={
           "waves-effect waves-light btn" + (if (valid.bind) "" else " disabled")
           }
          onclick={_:Event => onCreate() }>Create</a>
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
    <div>
      { trainingButtons.bind } <br />

      <div class="row">
        <div class="col s6">
          <h6 class="center-align">
            {
            if (isTraining.bind) s"Training Speed: ${GAME_SPEED_VALUES(gameSpeed.bind)}"
            else "Training Paused"
            }
          </h6>
        </div>
        <div class="col s6">
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