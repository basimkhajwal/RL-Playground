package rlp.pages

import com.thoughtworks.binding.{Binding, dom}
import com.thoughtworks.binding.Binding.{BindingSeq, Constant, Var, Vars}
import org.scalajs.dom.{Event, document, html}
import org.scalajs.dom.html.Div
import rlp._
import rlp.dao.ModelDAO
import rlp.models.Model
import rlp.ui.SelectHandler
import rlp.utils.Logger

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js

class ModelBuilder[A](
  builders: List[Model.Builder[A]],
  models: Vars[Model[A]],
  modelDAO: ModelDAO
) {

  private val modelSelect = new SelectHandler("Model Type", builders.map(_._1), Constant(false))

  private val modelCache: Vars[Model[A]] = Vars()

  private val modelBinding = Binding {
    val idx = modelSelect.selectedIndex.bind
    val (name, builder) = builders(idx)
    val cache = modelCache.bind

    cache.find(_.agentName equals name) match {
      case Some(model) => model
      case None => {
        val model = builder()
        modelCache.get += model
        model
      }
    }
  }

  private val modelName: Var[String] = Var(findUnusedName())

  private val validName: Var[Boolean] = Var(true)
  private val valid: Binding[Boolean] = Binding {
    validName.bind && modelBinding.bind.buildValid.bind
  }

  private def findUnusedName(): String = {
    val names = models.get.map(_.modelName.get)
    var idx = 1
    while (names contains ("Model"+idx)) idx += 1
    "Model"+idx
  }

  private def onNameChange(): Unit = {
    val modelNames = models.get.map(_.modelName.get)
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
  private def onCreate(): Unit = {

    val model = modelBinding.bind
    model.agent // Call build model
    model.modelName := modelName.get

    Logger.log("ModelBuilder", "Creating model " + model.toString)

    modelDAO.create(model.store()) map { id =>
      model.setId(id)
      models.get += model
    }

    reset()
    onClose()
  }

  private def reset(): Unit = {
    modelCache.get.clear()
    modelSelect.selectedIndex := 0
    modelName := findUnusedName()
    validName := true
  }

  private def onClose(): Unit = {
    js.Dynamic.global.$("#builder-modal").modal("close")
  }

  private def cloneModel(model: Model[A]): Unit = {

    val builderIdx = builders.indexWhere(_._1 equals model.agentName)
    modelSelect.selectedIndex := builderIdx

    val newModel = modelCache.get.find(_.agentName equals model.agentName).get
    newModel.cloneBuildFrom(model)

    onNameChange()
  }

  @dom
  private lazy val innerContent: Binding[Div] = {
    <div class="row">
      <div class="col s2 offset-s5">
        <span class="card-title center-align" onclick={_:Event => onClose()}>Create Model</span>
      </div>

      <div class="col s3 offset-s1">
        <a class={"dropdown-button btn-flat" + (if (models.bind.isEmpty) " disabled" else "")}
           href="#"
           data:data-activates="clone-dropdown" data:data-constrainwidth="false">
          <i class="material-icons left">arrow_drop_down_circle</i>Clone Existing
        </a>

        <ul id="clone-dropdown" class="dropdown-content">
          {
          for (model <- models) yield {
            <li>
              <a href="#" onclick={_:Event => cloneModel(model)}>{model.toString}</a>
            </li>
          }
          }
        </ul>
      </div>

      <div class="col s1">
        <span class="card-title right" id="close-button" onclick={_:Event => onClose()}>
          <i class="material-icons">close</i>
        </span>
      </div>

      <div class="col s12" style={"height:20px"}></div>

      <div class="col s3 offset-s2">
        { modelSelect.handler.bind }
      </div>

      <div class="col s3 offset-s2 input-field">
        <input id="model-name" class="validate" type="text"
               value={modelName.bind} onchange={_:Event => onNameChange()} required={true}/>
        <label class="active" for="model-name" data:data-error="Model name empty or already exists">Model Name</label>
      </div>

      <div class="col s12">
        { modelBinding.bind.modelBuilder.bind }
      </div>
    </div>
  }

  @dom
  lazy val content: Binding[Div] = {

    initModal("builder-modal")

    <div class="modal modal-fixed-footer" id="builder-modal">

      <div class="modal-content">
        { innerContent.bind }
      </div>

      <div class="row modal-footer">
        <div class="col s2 offset-s4">
          <a class="waves-effect waves-light btn" onclick={_:Event => onClose()}>Cancel</a>
        </div>

        <div class="col s2">
          <a class={ "waves-effect waves-light btn" + (if (valid.bind) "" else " disabled") }
             onclick={_:Event => onCreate() }>Create</a>
        </div>
      </div>
    </div>
  }

}
