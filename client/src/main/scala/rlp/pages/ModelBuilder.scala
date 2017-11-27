package rlp.pages

import com.thoughtworks.binding.{Binding, dom}
import com.thoughtworks.binding.Binding.{BindingSeq, Constant, Var, Vars}
import org.scalajs.dom.{Event, html}
import org.scalajs.dom.html.Div
import rlp.SelectHandler
import rlp.controllers.ModelController
import rlp._

class ModelBuilder[A](
  builders: List[ModelController.Builder[A]],
  models: Vars[ModelController[A]]
) {

  val newModelSelect = new SelectHandler("Model Type", builders.map(_._1), Constant(false))

  val controllerCache: Vars[(Int, ModelController[A])] = Vars()

  val modelController = Binding {
    val idx = newModelSelect.selectedIndex.bind
    val builder = builders(idx)._2
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
    val controller = modelController.bind
    controller.agent // Call build model
    controller.modelName := modelName.get

    models.get += controller
    onClose()
  }

  private def reset(): Unit = {
    controllerCache.get.clear()
    newModelSelect.selectedIndex := 0
    modelName := findUnusedName()
    validName := true
  }

  private def onClose(): Unit = {
    reset()
    getElem[html.Span]("close-button").click()
  }

  private def cloneModel(model: ModelController[A]): Unit = {

    val newController = model.cloneBuild()

    // Find builder index for this controller type
    val idx = builders.indexWhere(_._1 == newController.controllerName)

    // Inject controller into cache then fake selecting it
    val existingIdx = controllerCache.get.indexWhere(_._1 == idx)
    if (existingIdx >= 0) controllerCache.get.remove(existingIdx)
    controllerCache.get += ((idx, newController))
    newModelSelect.selectedIndex := idx
  }

  @dom
  lazy val content: Binding[Div] = {
    <div class="row">

      <div class="col s2 offset-s5">
        <span class="card-title center-align">Create New</span>
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
        <span class="card-title right" id="close-button"><i class="material-icons">close</i></span>
      </div>

      <div class="col s12" style={"height:20px"}></div>

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

}
