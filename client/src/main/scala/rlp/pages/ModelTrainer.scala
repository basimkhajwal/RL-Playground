package rlp.pages

import com.thoughtworks.binding.{Binding, dom}
import com.thoughtworks.binding.Binding.{Var, Vars}
import org.scalajs.dom.{Blob, Event, html, window}
import org.scalajs.dom.html.Div
import org.scalajs.dom.raw.{BlobPropertyBag, FileReader}
import rlp.environment.Environment
import rlp.models.Model
import rlp.utils.{BackgroundProcess, Logger}
import rlp._
import rlp.dao.ModelDAO
import rlp.storage.ModelStore
import rlp.ui.SelectHandler

import scala.scalajs.js

class ModelTrainer[A](
  models: Vars[Model[A]],
  builders: List[Model.Builder[A]],
  modelDAO: ModelDAO,
  trainStep: () => Unit,
) {

  val gameSpeedMultiplier = List(1, 2, 4, 6, -1)
  val gameSpeedToString = gameSpeedMultiplier.init.map("x"+_) ++ List("Max")

  val isTraining: Var[Boolean] = Var(false)
  val gameSpeed: Var[Int] = Var(0)

  val modelSelect = new SelectHandler("Model Select",
    models.mapBinding(m => Binding { m.agentName + " - " + m.modelName.bind })
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

      <div class="row grey col s12 lighten-3" id="model-training-header">
        <div class="col s12 center-align">
          <span class="card-title">Model Training</span>
        </div>

        <div class="row col s12 vertical-stretch-row">
          <div class="col s3 offset-s1">
            { modelSelect.handler.bind }
          </div>

          <div id="modal-btns" class="col s2 valign-wrapper">
            <a class="btn-floating waves-effect waves-light modal-trigger tooltipped red"
               href="#builder-modal" id="add-model-btn" data:data-tooltip="New Model">
              <i class="material-icons">add</i>
            </a>
            <a class="btn-floating waves-effect waves-light modal-trigger tooltipped red lighten-2"
               href="#import-modal" id="import-model-btn" data:data-tooltip="Import Model">
              <i class="material-icons">file_upload</i>
            </a>
          </div>

          <div class="col s3 valign-wrapper" id="training-btns-container">{ trainingButtons.bind }</div>
        </div>
      </div>

      <div class="col s12 grey lighten-4 row" id="model-info">
        {
          selectedModel.bind match {
            case Some(model) => modelEditPane(model).bind
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

      { importModal.bind }
    </div>
  }

  @dom
  private def modelEditPane(model: Model[A]): Binding[html.Element] = {

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
      import upickle.default._
      val fileStore = write(model.store())
      val fileBlob = new Blob(js.Array(fileStore))

      js.Dynamic.global.saveAs(fileBlob, model.toString + ".json")
    }

    val MIN_SAVE_DELAY = 1000
    var lastSaveTime: Double = 0
    var saveScheduled: Boolean = false
    var forceSave: Boolean = true

    def checkSave(): Unit = {
      if (forceSave || model.viewDirty) {
        modelDAO.update(model.store())
        model.resetViewDirty()

        saveScheduled = false
        forceSave = false
        lastSaveTime = window.performance.now()

        Logger.log("ModelTrainer", model.toString + " saved")
      }
    }

    def scheduleSave(force: Boolean = false): Unit = {
      if (!saveScheduled) {
        val timeOutDelay = Math.max(50, (MIN_SAVE_DELAY + lastSaveTime) - window.performance.now())
        js.timers.setTimeout(timeOutDelay) { checkSave() }
        saveScheduled = true

        Logger.log("ModelTrainer", "Save scheduled in " + timeOutDelay + "ms")
      }
      forceSave |= force
    }

    def trainingChanged(isTraining: Boolean): Unit = {
      if (!isTraining) {
        scheduleSave(true)
      }
    }

    def onSubmitLeaderboard(): Unit = {
      // TODO
    }

    initScript("model-option-btns") { () => js.Dynamic.global.$(".tooltipped").tooltip() }

    <div class="row col s10 offset-s1">
      <div class="input-field col s3">
        <input id="model-name-train" class="validate" type="text"
               value={model.modelName.bind} onchange={_:Event => onNameChange()} required={true}/>
        <label for="model-name-train" data:data-error="Model name empty or already exists">Model Name</label>
      </div>

      <div class="col s3 center-align">
        <h6 id="episode-label">Episodes Trained</h6>
        <h6><strong>{model.gamesPlayed.bind.toString}</strong></h6>
      </div>

      <div id="model-option-btns" class="col s4 offset-s1">
        <a class="btn-floating waves-effect waves-light tooltipped brown lighten-1"
           data:data-tooltip="Submit to leaderboard"
           onclick={_:Event => onSubmitLeaderboard()}>
          <i class="material-icons">publish</i>
        </a>
        <a class="btn-floating waves-effect waves-light tooltipped blue-grey"
           data:data-tooltip="Export file"
           onclick={_:Event => onExport()}>
          <i class="material-icons">file_download</i>
        </a>
        <a class="btn-floating waves-effect waves-light tooltipped red"
           data:data-tooltip="Delete model"
           onclick={_:Event => onDelete() }>
          <i class="material-icons">delete</i>
        </a>

        {
          trainingChanged(isTraining.bind)
          ""
        }

      </div>

    </div>
  }

  @dom
  private lazy val importModal: Binding[html.Element] = {

    val importError = Var("")

    def onImport(): Unit = {

      val fileElem = getElem[html.Input]("import-file")

      if (fileElem.files.length == 0) {
        importError := "Error - No file specified"
        return
      }

      try {
        val file = fileElem.files(0)

        val reader = new FileReader()
        reader.readAsText(file)

        reader.onload = { _ =>

          try {
            val result = reader.result.asInstanceOf[String]
            val store = upickle.default.read[ModelStore](result)

            builders.find(_._1 == store.agentName) match {

              case Some((_, builder)) => {
                val model = builder()
                model.load(store)
                models.get += model

                js.Dynamic.global.$("#import-modal").modal("close")
              }

              case None => importError := s"Error reading data, invalid agent ${store.agentName}"
            }
          } catch {
            case e: Exception => {
              importError := "Error parsing file: \n" + e.getMessage
            }
          }
        }

      } catch {
        case e: Exception => {
          importError := "Error reading file: \n" + e.getMessage
        }
      }
    }

    initModal("import-modal")

    <div class="modal" id="import-modal">

      <div class="modal-content">
        <span class="card-title center-align">Import Model</span>
        <form action="#">
          <div class="file-field input-field">
            <div class="btn waves-effect teal lighten-2">
              <span>File</span>
              <input type="file" id="import-file"/>
            </div>
            <div class="file-path-wrapper">
              <input class="file-path validate" type="text" placeholder="Choose file" />
            </div>
          </div>
        </form>

        <h5 class="center-align red-text">{ if (importError.bind != "") "Error" else "" }</h5>
        <h6 class="center-align red-text lighten-2">{importError.bind}</h6>
      </div>

      <div class="modal-footer valign-wrapper">
        <a class="btn waves-effect waves-light deep-orange lighten-2" onclick={_:Event => onImport()}>Import</a>
      </div>
    </div>
  }

  @dom
  private lazy val trainingButtons: Binding[html.Div] = {
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
