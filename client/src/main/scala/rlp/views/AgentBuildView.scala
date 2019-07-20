package rlp.views

import com.thoughtworks.binding.Binding.{Constant, Var, Vars}
import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.html.Div
import org.scalajs.dom.{Event, html}
import rlp._
import rlp.dao.LocalAgentDAO
import rlp.presenters.AgentPresenter
import rlp.ui.SelectHandler
import rlp.utils.Logger

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js

/**
  * Handles the build modal for creating new agents
  *
  * @param builders
  * @param agents
  * @tparam A
  */
class AgentBuildView[A](
  builders: List[AgentPresenter.Builder[A]],
  agents: Vars[AgentPresenter[A]],
) {

  private val builderSelect = new SelectHandler("Agent Type", builders.map(_._1), Constant(false))

  /**
    * A cache for presenters if the user navigates away from the modal
    * or to another presenter
    */
  private val presenterCache: Vars[AgentPresenter[A]] = Vars()

  private val presenterBinding = Binding {

    val idx = builderSelect.selectedIndex.bind
    val (name, builder) = builders(idx)
    val cache = presenterCache.bind

    cache.find(_.agentName equals name) match {
      case Some(agent) => agent
      case None => {
        val presenter = builder()
        presenterCache.get += presenter
        presenter
      }
    }
  }

  private val name: Var[String] = Var(findUnusedName())

  private val validName: Var[Boolean] = Var(true)
  private val valid: Binding[Boolean] = Binding {
    validName.bind && presenterBinding.bind.buildValid.bind
  }

  private def findUnusedName(): String = {
    val names = agents.get.map(_.name.get)
    var idx = 1
    while (names contains ("Agent"+idx)) idx += 1
    "Agent"+idx
  }

  private def onNameChange(): Unit = {
    val agentNames = agents.get.map(_.name.get)
    val nameElem = getElem[html.Input]("agent-name")

    if (agentNames contains nameElem.value) {
      nameElem.setCustomValidity("Invalid")
      validName := false
    } else {
      nameElem.setCustomValidity("")
      name := nameElem.value
      validName := true
    }
  }

  @dom
  private def onCreate(): Unit = {

    val presenter = presenterBinding.bind
    presenter.agent // Call build
    presenter.name := name.get

    Logger.log("AgentBuildView", "Creating agent " + presenter.toString)

    LocalAgentDAO.create(presenter.store()) map { id =>
      presenter.setId(id)
      agents.get += presenter
    }

    reset()
    onClose()
  }

  private def reset(): Unit = {
    presenterCache.get.clear()
    builderSelect.selectedIndex := 0
    name := findUnusedName()
    validName := true
  }

  private def onClose(): Unit = {
    js.Dynamic.global.$("#builder-modal").modal("close")
  }

  @dom
  private lazy val innerContent: Binding[Div] = {
    <div class="row">
      <div class="col s2 offset-s5">
        <span class="card-title center-align" onclick={_:Event => onClose()}>Create Agent</span>
      </div>

      <div class="col s1 offset-s4">
        <span class="card-title right" id="close-button" onclick={_:Event => onClose()}>
          <i class="material-icons">close</i>
        </span>
      </div>

      <div class="col s12" style={"height:20px"}></div>

      <div class="col s3 offset-s2">
        { builderSelect.handler.bind }
      </div>

      <div class="col s3 offset-s2 input-field">
        <input id="agent-name" class="validate" type="text"
               value={name.bind} onchange={_:Event => onNameChange()} required={true}/>
        <label class="active" for="agent-name"
               data:data-error="Agent name empty or already exists">
          Agent Name
        </label>
      </div>

      <div class="col s12">
        { presenterBinding.bind.agentBuilder.bind }
      </div>
    </div>
  }

  @dom
  lazy val content: Binding[Div] = {

    val onOpen: js.Function = { () =>
      name := findUnusedName()
    }

    initModal("builder-modal",
      js.Dynamic.literal(
        "ready" -> onOpen
      )
    )

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
