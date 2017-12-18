package rlp.pages

import com.thoughtworks.binding.{Binding, dom}
import com.thoughtworks.binding.Binding.{Var, Vars}
import org.scalajs.dom.{Event, html}
import org.scalajs.dom.html.Div
import rlp.environment.Environment
import rlp.models.Model
import rlp.utils.{BackgroundProcess, SelectHandler}
import rlp._

import scala.scalajs.js

class ModelTrainer[A](
  models: Vars[Model[A]],
  trainStep: () => Unit,
) {

  val gameSpeedMultiplier = List(1, 2, 4, 6, -1)
  val gameSpeedToString = gameSpeedMultiplier.init.map("x"+_) ++ List("Max")

  val isTraining: Var[Boolean] = Var(false)
  val gameSpeed: Var[Int] = Var(0)

  val modelSelect = new SelectHandler("Model Select",
    models.mapBinding(m => Binding { m.controllerName + " - " + m.modelName.bind })
  )

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

  def startTraining(): Unit = {
    trainingProcess.start(Environment.FPS * gameSpeedMultiplier(gameSpeed.get))
    isTraining := true
  }

  def pauseTraining(): Unit = {
    trainingProcess.stop()
    isTraining := false
  }

  private def modelSelected(model: Model[A]): Unit = {
    if (isTraining.get) pauseTraining()
    js.timers.setTimeout(100) { js.Dynamic.global.Materialize.updateTextFields() }
  }

  @dom
  private def resetTraining(): Unit = {
    selectedModel.bind.get.resetAgent()
  }

  private def fastForwardTraining(): Unit = {
    gameSpeed := (gameSpeed.get + 1) % gameSpeedMultiplier.length
    trainingProcess.stop()
    trainingProcess.start(Environment.FPS * gameSpeedMultiplier(gameSpeed.get))
  }

  @dom
  lazy val content: Binding[Div] = {

    <div class="row" id="model-trainer">

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

      <div class="col s10 offset-s1">
        {
          selectedModel.bind match {
            case Some(model) => {

              def onNameChange(): Unit = {
                val modelNames = models.get.map(_.modelName.get)
                val modelNameElem = getElem[html.Input]("model-name-train")

                if (modelNames contains modelNameElem.value) {
                  modelNameElem.setCustomValidity("Invalid")
                } else {
                  modelNameElem.setCustomValidity("")
                  model.modelName := modelNameElem.value
                }
              }

              def onDelete(): Unit = {
                modelSelect.selectedIndex := 0
                models.get.remove(models.get.indexOf(model))
              }

              def onExport(): Unit = {
                // TOOD: Add model exporting and serialization within models
              }

              <div class="row">
                <div class="input-field col s3">
                  <input id="model-name-train" class="validate" type="text"
                         value={model.modelName.bind} onchange={_:Event => onNameChange()} required={true}/>
                  <label for="model-name-train" data:data-error="Model name empty or already exists">Model Name</label>
                </div>

                <h6 class="center-align col s3">{s"Games Played: ${model.gamesPlayed.bind}"}</h6>

                <a class="btn waves-effect waves-light col s3"
                   onclick={_:Event => onExport()}>Export</a>
                <a class="btn waves-effect waves-light col s3"
                   onclick={_:Event => onDelete() }>Delete</a>
              </div>
            }
            case None => <!-- -->
          }
        }
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
