package rlp.views

import com.thoughtworks.binding.Binding.{Var, Vars}
import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.html.Div
import org.scalajs.dom.raw.FileReader
import org.scalajs.dom.{Blob, Event, html, window}
import rlp._
import rlp.dao.LocalAgentDAO
import rlp.environment.Environment
import rlp.presenters.{AgentPresenter, AgentStore}
import rlp.ui.SelectHandler
import rlp.utils.{BackgroundProcess, Logger}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js

/**
  * The largest view which incorporates the agent training
  * and hyper-parameter selection
  *
  * @param agents
  * @param builders
  * @param agentPerformance
  * @param trainStep
  * @tparam A
  */
class AgentTrainView[A](
  agents: Vars[AgentPresenter[A]],
  builders: List[AgentPresenter.Builder[A]],
  agentPerformance: (AgentPresenter[A]) => Double,
  trainStep: () => Unit,
) {

  // Multipliers for the fast forward option
  val gameSpeedMultiplier = List(1, 2, 4, 6, -1)
  val gameSpeedToString = gameSpeedMultiplier.init.map("x"+_) ++ List("Max")

  //    Various bindings to the training state

  val isTraining: Var[Boolean] = Var(false)
  val gameSpeed: Var[Int] = Var(0)

  val agentSelect = new SelectHandler("Agent Select",
    agents.mapBinding(a => Binding { a.agentName + " - " + a.name.bind })
  )

  val agentExists = Binding { agents.bind.nonEmpty }

  val selectedAgent: Binding[Option[AgentPresenter[A]]] = Binding {
    if (agentExists.bind) {
      val agent = agents.bind(agentSelect.selectedIndex.bind)
      agentSelected(agent)
      Some(agent)
    } else {
      None
    }
  }

  private val trainingProcess = new BackgroundProcess(trainStep, "Training")

  /* Training control methods */

  def startTraining(): Unit = {
    trainingProcess.start(Environment.FPS * gameSpeedMultiplier(gameSpeed.get))
    isTraining := true
  }

  def pauseTraining(): Unit = {
    trainingProcess.stop()
    isTraining := false
  }

  /**
    * When a new agent is selected, stop training and update
    * the materialize ui
    *
    * @param agent
    */
  private def agentSelected(agent: AgentPresenter[A]): Unit = {
    if (isTraining.get) pauseTraining()
    js.timers.setTimeout(100) {
      js.Dynamic.global.Materialize.updateTextFields()
    }
  }

  @dom
  private def resetTraining(): Unit = {
    selectedAgent.bind.get.resetAgent()
  }

  private def fastForwardTraining(): Unit = {
    gameSpeed := (gameSpeed.get + 1) % gameSpeedMultiplier.length
    trainingProcess.stop()
    trainingProcess.start(Environment.FPS * gameSpeedMultiplier(gameSpeed.get))
  }

  @dom
  lazy val trainingHeader: Binding[Div] = {
    <div id="agent-training-header">
      <!-- Top area controls -->
      <div class="row col s12 vertical-stretch-row">

        <div class="col s3 offset-s1">
          { agentSelect.handler.bind }
        </div>

        <div id="modal-btns" class="col s2 valign-wrapper">

          <!-- Add new model button -->
          <a class="btn-floating waves-effect waves-light modal-trigger tooltipped red"
             href="#builder-modal" id="add-agent-btn" data:data-tooltip="New Agent">
            <i class="material-icons">add</i>
          </a>

          <!-- Upload model from file button -->
          <a class="btn-floating waves-effect waves-light modal-trigger tooltipped red lighten-2"
             href="#import-modal" id="import-agent-btn" data:data-tooltip="Import Agent">
            <i class="material-icons">file_upload</i>
          </a>
        </div>

        <div class="col s3 valign-wrapper" id="training-btns-container">
          { trainingButtons.bind }
        </div>
      </div>
    </div>
  }

  @dom
  lazy val content: Binding[Div] = {

    <div class="row" id="agent-trainer">

      <!-- Generic agent edit pane, same for all agents -->
      <div class="col s12 grey lighten-4 row" id="agent-info">
        <!-- Title -->
        <div class="col s12 center-align">
          <span class="card-title" style="margin-bottom: 15px">Agent Hyperparameters</span>
        </div>
        {
          selectedAgent.bind match {
            case Some(agent) => agentEditPane(agent).bind
            case None => <!-- -->
          }
        }
      </div>

      <!-- Agent specific edit pane (bind from presenter) -->
      <div class="col s12">
        {
          selectedAgent.bind match {
            case Some(agent) => agent.agentViewer.bind
            case None => {
              <p class="center-align">
                Create an agent above to edit hyper-parameters.
              </p>
            }
          }
        }
      </div>

      { importModal.bind }
    </div>
  }

  @dom
  private def agentEditPane(agent: AgentPresenter[A]): Binding[html.Element] = {

    val MIN_SAVE_DELAY = 1000
    var lastSaveTime: Double = 0
    var saveScheduled: Boolean = false
    var forceSave: Boolean = true
    var deleteIssued: Boolean = false

    def onNameChange(): Unit = {
      val agentNames = agents.get.map(_.name.get)
      val nameElem = getElem[html.Input]("agent-name-train")

      if (agentNames contains nameElem.value) {
        nameElem.setCustomValidity("Invalid")
      } else {
        nameElem.setCustomValidity("")
        agent.name := nameElem.value
      }
    }

    def onDelete(): Unit = {

      Logger.log("AgentTrainView", "Deleting agent - " + agent.toString)

      agentSelect.selectedIndex := 0
      agents.get.remove(agents.get.indexOf(agent))

      deleteIssued = true

      LocalAgentDAO.delete(agent.id) recover {
        case e:Throwable => Logger.log("AgentTrainView", "Delete error - " + e.getMessage)
      }

      js.timers.setTimeout(100) {
        js.Dynamic.global.$(".material-tooltip").attr("style", "")
      }
    }

    def onExport(): Unit = {
      import upickle.default._
      val fileStore = write(agent.store())
      val fileBlob = new Blob(js.Array(fileStore))

      js.Dynamic.global.saveAs(fileBlob, agent.toString + ".json")
    }

    def checkSave(): Unit = {
      if ((forceSave || agent.viewDirty) && !deleteIssued) {

        LocalAgentDAO.update(agent.store()) recover {
          case e:Throwable => Logger.log("AgentTrainView", "Save error " + e.getMessage)
        }

        agent.resetViewDirty()

        saveScheduled = false
        forceSave = false
        lastSaveTime = window.performance.now()

        Logger.log("AgentTrainView", agent.toString + " saved")
      }
    }

    def scheduleSave(force: Boolean = false): Unit = {
      if (!saveScheduled) {

        val timeOutDelay = Math.max(50, (MIN_SAVE_DELAY + lastSaveTime) - window.performance.now())
        js.timers.setTimeout(timeOutDelay) { checkSave() }

        saveScheduled = true

        Logger.log("AgentTrainView", "Save scheduled in " + timeOutDelay + "ms")
      }
      forceSave |= force
    }

    def trainingChanged(isTraining: Boolean): Unit = {
      if (!isTraining) {
        scheduleSave(true)
      }
    }

    def viewChanged(viewDirty: Boolean): Unit = {
      if (viewDirty) {
        scheduleSave(false)
      }
    }

    initScript("agent-option-btns") { () => js.Dynamic.global.$(".tooltipped").tooltip() }

    <div class="row col s10 offset-s1">

      <div class="input-field col s3">
        <input id="agent-name-train" class="validate" type="text"
               value={agent.name.bind} onchange={_:Event => onNameChange()} required={true}/>
        <label for="agent-name-train" data:data-error="Agent name empty or already exists">Agent Name</label>
      </div>

      <div class="col s3 center-align">
        <h6 id="episode-label">Episodes Trained</h6>
        <h6><strong>{agent.gamesPlayed.bind.toString}</strong></h6>
      </div>

      <div id="agent-option-btns" class="col s4 offset-s1">
        <!-- <a class="btn-floating waves-effect waves-light tooltipped brown lighten-1 modal-trigger"
           data:data-tooltip="Submit to leaderboard" href="#leaderboard-modal">
          <i class="material-icons">publish</i>
        </a> -->
        <a class="btn-floating waves-effect waves-light tooltipped blue-grey"
           data:data-tooltip="Export file"
           onclick={_:Event => onExport()}>
          <i class="material-icons">file_download</i>
        </a>
        <a class="btn-floating waves-effect waves-light tooltipped red"
           data:data-tooltip="Delete agent"
           onclick={_:Event => onDelete() }>
          <i class="material-icons">delete</i>
        </a>

        { leaderboardModal(agent).bind }

        {
          viewChanged(agent.viewDirtyBinding.bind)
          trainingChanged(isTraining.bind)
          ""
        }

      </div>

    </div>
  }

  @dom
  private def leaderboardModal(presenter: AgentPresenter[A]): Binding[html.Element] = {

    val numRuns = 200

    val agentScore = Var[Double](Double.NaN)
    var totalScore = 0.0
    val runsCompleted = Var[Int](0)
    val completed = Var(false)

    val submitError = Var("")

    var scoreProcess: BackgroundProcess = null

    def onSubmit(): Unit = {
      // TODO: Submit score to server
    }

    def stepScore(): Unit = {
      totalScore += agentPerformance(presenter)

      runsCompleted := runsCompleted.get + 1
      runsCompletedChanged()

      if (runsCompleted.get == numRuns) {
        scoreProcess.stop()
        agentScore := totalScore / numRuns
        completed := true
      }
    }

    def runsCompletedChanged(): Unit = {
      js.Dynamic.global.$("#leaderboard-progress").width((100 * runsCompleted.get)/numRuns + "%")
    }

    val onOpen: js.Function = { () =>
      scoreProcess.start(40)
    }

    val onClose: js.Function = { () =>
      if (runsCompleted.get != numRuns) {
        scoreProcess.stop()
      }
      totalScore = 0
      runsCompleted := 0
      completed := false
      submitError := ""
      runsCompletedChanged()
    }

    initModal("leaderboard-modal",
      js.Dynamic.literal(
        "ready" -> onOpen,
        "complete" -> onClose
      )
    )

    scoreProcess = new BackgroundProcess(stepScore, "Leaderboard Score")

    <div class="modal" id="leaderboard-modal">
      <div class="modal-content">
        <span class="card-title center-align">Submit to Leaderboard</span>

        <br />
        <h6 class="center-align black-text lighten-1">
          Agent Entry - <strong>{presenter.toString}</strong>
        </h6>
        <br />
        <br />

        <h6 class="center-align">
          {
            if (completed.bind) "Score: " + agentScore.bind.toString
            else "Calculating score..."
          }
        </h6>

        <div class="progress">
          <div class="determinate" id="leaderboard-progress"></div>
        </div>

        {
          submitError.bind match {
            case "" => <!-- -->
            case error =>
              <h6 class="center-align red-text lighten-1"><strong>Error - </strong>{error}</h6>
          }
        }

      </div>

      <div class="modal-footer valign-wrapper">
        <a class={
             "btn waves-effect waves-light red lighten-2" +
             (if (completed.bind) "" else " disabled")
           }
           onclick={_:Event => onSubmit()}>Submit</a>
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
            val store = upickle.default.read[AgentStore](result)

            builders.find(_._1 == store.agentName) match {

              case Some((_, builder)) => {
                val agent = builder()
                agent.load(store)
                agents.get += agent

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
        <span class="card-title center-align">Import Agent</span>
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

        {
          importError.bind match {
            case "" => <!-- -->
            case error =>
              <h6 class="center-align red-text lighten-1"><strong>Error - </strong>{error}</h6>
          }
        }
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
      (if (agentExists.bind) "" else "disabled ")
    val training = isTraining.bind

    <div class="center-align" id="buttons-container">
      <div class="valign-wrapper">

        <!-- Reset button -->
        <a class= {buttonStyle + "btn-medium orange tooltipped"}
           onclick={ _:Event => resetTraining() }
           data:data-tooltip="Reset agent">
          <i class="material-icons">replay</i>
        </a>

        <!-- Pause/Play button -->
        <a class={buttonStyle + "btn-large red"}
           onclick = { _:Event => if (training) pauseTraining() else startTraining() }>
          <i class="material-icons">
            { if (training) "pause" else "play_arrow" }
          </i>
        </a>

        <!-- Fast forward button -->
        <a class= { buttonStyle + "btn-medium orange " + (if (training) "" else "disabled") }
           onclick={ _:Event => fastForwardTraining() }>
          <i class="material-icons">fast_forward</i>
        </a>
        <span id="training-speed">
          { if (isTraining.bind) gameSpeedToString(gameSpeed.bind) else "" }
        </span>
      </div>
    </div>
  }
}
