package rlp.pages

import com.thoughtworks.binding.{Binding, dom}
import com.thoughtworks.binding.Binding.{Constant, Var, Vars}
import org.scalajs.dom.Event
import org.scalajs.dom.html.Div
import rlp.environment.Environment
import rlp.models.Model
import rlp.utils.{BackgroundProcess, SelectHandler}

import scala.scalajs.js

class ModelTrainer[A](
  models: Vars[Model[A]],
  trainStep: () => Unit,
) {

  val gameSpeedMultiplier = List(1, 2, 5, 10, -1)
  val gameSpeedToString = gameSpeedMultiplier.init.map("x"+_) ++ List("Max")

  val isTraining: Var[Boolean] = Var(false)
  val gameSpeed: Var[Int] = Var(0)

  val modelSelect = new SelectHandler("Model Select", models.map(_.toString), Constant(false))

  val modelExists = Binding { models.bind.nonEmpty }

  val selectedModel: Binding[Option[Model[A]]] = Binding {
    if (modelExists.bind) {
      val model = models.bind(modelSelect.selectedIndex.bind)
      modelSelected(model)
      Some(model)
    } else {
      None
    }
  }

  private val trainingProcess = new BackgroundProcess(trainStep, "Training")

  private def startTraining(): Unit = {
    trainingProcess.start(Environment.FPS * gameSpeedMultiplier(gameSpeed.get))
    isTraining := true
  }

  private def pauseTraining(): Unit = {
    trainingProcess.stop()
    isTraining := false
  }

  private def modelSelected(model: Model[A]): Unit = {
    if (isTraining.get) pauseTraining()
    js.timers.setTimeout(100) { js.Dynamic.global.Materialize.updateTextFields() }
  }

  @dom
  private def resetTraining(): Unit = {
    val model = selectedModel.bind.get

    model.resetAgent()
    model.gamesPlayed := 0
  }

  private def fastForwardTraining(): Unit = {
    gameSpeed := (gameSpeed.get + 1) % gameSpeedMultiplier.length
    trainingProcess.stop()
    trainingProcess.start(Environment.FPS * gameSpeedMultiplier(gameSpeed.get))
  }

  @dom
  lazy val content: Binding[Div] = {

    <div class="row">

      <div class="row grey col s12 lighten-3" id="model-training">
        <div class="col s2">
          <span class="card-title">Model Training</span>
        </div>

        <div class="col s3"> { modelSelect.handler.bind } </div>

        <div class="col s3"> { trainingButtons.bind } </div>

        <div class="col s4" id="model-training-btns">
          {
          val btnStyle = "btn waves-effect waves-light"

          <a class={btnStyle + " activator"}>New</a>
          }
        </div>
      </div>

      <div class="col s12">
        <h6 class="center-align">
          {
          selectedModel.bind match {
            case Some(m) => s"Games Played: ${m.gamesPlayed.bind}"
            case None => ""
          }
          }
        </h6>
      </div>

      <div class="col s12">
        {
        selectedModel.bind match {
          case Some(model) => model.modelViewer.bind
          case None => <!-- -->
        }
        }
      </div>

    </div>
  }

  @dom
  private lazy val trainingButtons: Binding[Div] = {
    val buttonStyle =
      "center-align btn-floating waves-effect waves-circle " +
      (if (modelExists.bind) "" else "disabled ")
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
}
