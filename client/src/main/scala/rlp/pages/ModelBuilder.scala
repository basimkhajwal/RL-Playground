package rlp.pages

import com.thoughtworks.binding.{Binding, dom}
import com.thoughtworks.binding.Binding.{BindingSeq, Constant, Var, Vars}
import org.scalajs.dom.{Event, html}
import org.scalajs.dom.html.Div
import rlp._
import rlp.models.Model
import rlp.utils.SelectHandler

class ModelBuilder[A](
  builders: List[Model.Builder[A]],
  models: Vars[Model[A]]
) {

  private val modelSelect = new SelectHandler("Model Type", builders.map(_._1), Constant(false))

  private val modelCache: Vars[(Int, Model[A])] = Vars()

  private val modelBinding = Binding {
    val idx = modelSelect.selectedIndex.bind
    val builder = builders(idx)._2
    val cache = modelCache.get

    cache.find(c => c._1 == idx) match {
      case Some((_, model)) => model
      case None => {
        val model = builder()
        cache += ((idx, model))
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

    models.get += model
    onClose()
  }

  private def reset(): Unit = {
    modelCache.get.clear()
    modelSelect.selectedIndex := 0
    modelName := findUnusedName()
    validName := true
  }

  private def onClose(): Unit = {
    reset()
    getElem[html.Span]("close-button").click()
  }

  private def cloneModel(model: Model[A]): Unit = {

    val newModel = model.cloneBuild()

    // Find builder index for this controller type
    val builderIdx = builders.indexWhere(_._1 == newModel.controllerName)

    // Inject controller into cache then fake selecting it
    val cacheIdx = modelCache.get.indexWhere(_._1 == builderIdx)
    if (cacheIdx >= 0) modelCache.get.remove(cacheIdx)
    modelCache.get += ((builderIdx, newModel))
    modelSelect.selectedIndex := builderIdx
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
        { modelSelect.handler.bind }
      </div>

      <div class="col s3 offset-s2 input-field">
        <input id="model-name" class="validate" type="text"
               value={modelName.bind} onchange={_:Event => onNameChange()} required={true}/>
        <label for="model-name" data:data-error="Model name empty or already exists">Model Name</label>
      </div>

      <div class="col s12">
        { modelBinding.bind.modelBuilder.bind }
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
