package rlp.dao
import rlp.presenters.AgentStore
import rlp.utils.Logger
import upickle.default

import scala.concurrent.Future
import scala.scalajs.js
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Contains static functions to encapsulate access the IndexedDB
  * database in terms of AgentStore without exposing with
  * database details
  */
object LocalAgentDAO {

  private def log(msg: String): Unit = {
    Logger.log("LocalAgentDAO", msg)
  }

  /* ====== CRUD operations on the AgentStore class directly without DB access === */

  def getAll(): Future[Seq[AgentStore]] = {
    log("Retrieving model stores")
    IndexedDB.getAll[AgentStoreItem](IndexedDB.MODEL_STORE) map { items =>
      log(items.length + " items retrieved")
      items map { item => AgentStoreItem.extract(item) }
    }
  }

  def create(model: AgentStore): Future[Long] = {

    val modelID: Long = System.currentTimeMillis()
    val item = AgentStoreItem.fromStore(model)
    item.id = modelID

    log(s"Creating model store id $modelID from ${model.agentName} - ${model.name}")

    IndexedDB
      .create(IndexedDB.MODEL_STORE, item)
      .map { _ => modelID }
  }

  def update(model: AgentStore): Future[Unit] = {
    log(s"Updating model store id ${model.id}")

    IndexedDB
      .retrieve[AgentStoreItem](IndexedDB.MODEL_STORE, model.id.toDouble)
      .flatMap { item =>
        item.modelStore = default.write(model)
        IndexedDB.update(IndexedDB.MODEL_STORE, item)
      }
  }

  def delete(id: Long): Future[Unit] = {
    log(s"Deleting model store id $id")
    IndexedDB.delete(IndexedDB.MODEL_STORE, id.toDouble)
  }

  /**
    * Scala wrapper for Javascript object
    * used to store the agent data
    */
  @js.native
  trait AgentStoreItem extends js.Object {
    var id: Double = js.native
    var modelStore: String = js.native
  }

  /**
    * Collection of useful functions to handle
    * the agent data object wrapper and
    * convert to/from the AgentStore class
   */
  object AgentStoreItem {

    def extract(item: AgentStoreItem): AgentStore = {
      default.read[AgentStore](item.modelStore)
    }

    def apply(id: Long, modelStore: String): AgentStoreItem = {
      js.Dynamic.literal(
        "id" -> id,
        "modelStore" -> modelStore
      ).asInstanceOf[AgentStoreItem]
    }

    def fromStore(modelStore: AgentStore): AgentStoreItem = {
      apply(modelStore.id, default.write(modelStore))
    }
  }
}
