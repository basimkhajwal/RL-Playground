package rlp.dao
import rlp.models.Model
import org.scalajs.dom.{html, window}
import org.scalajs.dom.idb.{Database, OpenDBRequest}
import rlp.storage.ModelStore

import scala.concurrent.{Future, Promise}

object LocalModelDAO extends ModelDAO {

  private val DB_NAME = "rlp"

  private def getDatabase(): Future[Database] = {

    val dBRequest = window.indexedDB.open("rlp", 1)
    val promise = Promise[Database]()

    dBRequest.onupgradeneeded = { event =>
      val db = dBRequest.result.asInstanceOf[Database]

      val objectStore = db.createObjectStore("modelStore")


    }

    dBRequest.onerror = { err =>
      promise.failure(new Exception(err.message))
    }

    dBRequest.onsuccess = { _ =>
      promise.success(dBRequest.result.asInstanceOf[Database])
    }

    promise.future
  }

  override def getAll[A](): Future[Seq[ModelStore]] = {
    Future { List() }
  }

  override def persist(model: ModelStore): Future[Boolean] = {
    Future(false)
  }

  override def delete(model: ModelStore): Future[Boolean] = {
    Future(false)
  }
}
