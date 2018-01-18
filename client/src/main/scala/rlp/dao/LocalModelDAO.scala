package rlp.dao
import rlp.storage.ModelStore
import rlp.utils.Logger
import upickle.default

import scala.concurrent.Future
import scala.scalajs.js
import scala.concurrent.ExecutionContext.Implicits.global

object LocalModelDAO extends ModelDAO {

  private def log(msg: String): Unit = {
    Logger.log("LocalModelDAO", msg)
  }

  override def getAll(): Future[Seq[ModelStore]] = {
    log("Retrieving model stores")
    IndexedDB.getAll[ModelStoreItem](IndexedDB.MODEL_STORE) map { items =>
      log(items.length + " items retrieved")
      items map { item => ModelStoreItem.extract(item) }
    }
  }

  override def create(model: ModelStore): Future[Long] = {

    val modelID: Long = System.currentTimeMillis()
    val item = ModelStoreItem.fromStore(model)
    item.id = modelID

    log(s"Creating model store id $modelID from ${model.agentName} - ${model.modelName}")

    IndexedDB
      .create(IndexedDB.MODEL_STORE, item)
      .map { _ => modelID }
  }

  override def update(model: ModelStore): Future[Unit] = {
    log(s"Updating model store id ${model.id}")

    IndexedDB.retrieve[ModelStoreItem](IndexedDB.MODEL_STORE, model.id.toDouble) flatMap { item =>
      item.modelStore = default.write(model)
      IndexedDB.update(IndexedDB.MODEL_STORE, item)
    }
  }

  override def delete(id: Long): Future[Unit] = {
    log(s"Deleting model store id $id")
    IndexedDB.delete(IndexedDB.MODEL_STORE, id.toDouble)
  }

  @js.native
  trait ModelStoreItem extends js.Object {
    var id: Double = js.native
    var modelStore: String = js.native
  }

  object ModelStoreItem {

    def extract(item: ModelStoreItem): ModelStore = {
      default.read[ModelStore](item.modelStore)
    }

    def apply(id: Long, modelStore: String): ModelStoreItem = {
      js.Dynamic.literal(
        "id" -> id,
        "modelStore" -> modelStore
      ).asInstanceOf[ModelStoreItem]
    }

    def fromStore(modelStore: ModelStore): ModelStoreItem = {
      apply(modelStore.id, default.write(modelStore))
    }
  }
}
