package rlp.dao
import org.scalajs.dom.{ErrorEvent, Event, html, window}
import org.scalajs.dom.idb.{Database, Request}
import rlp.storage.ModelStore

import scala.concurrent.{Future, Promise}
import scala.scalajs.js

object LocalModelDAO extends ModelDAO {

  private val DB_NAME = "rlp"

  private def getDatabase(): Future[Database] = {

    val dBRequest = window.indexedDB.open("rlp", 1)

    dBRequest.onupgradeneeded = { event =>
      val db = dBRequest.result.asInstanceOf[Database]

      val modelStore = db.createObjectStore("modelStore", js.Dynamic.literal("keyPath" -> "id"))

      val failSafe = db.createObjectStore("failSafe", js.Dynamic.literal("keyPath" -> "id"))
      failSafe.createIndex("uid", "uid", js.Dynamic.literal("unique" -> false))
    }

    requestPromise(dBRequest) { _ =>
      dBRequest.result.asInstanceOf[Database]
    }
  }

  def requestPromise[A](request: Request)(success: Event => A): Future[A] = {
    requestPromise(request, success, err => new Exception(err.message))
  }

  def requestPromise[A](request: Request, success: Event => A, error: ErrorEvent => Throwable): Future[A] = {
    val promise = Promise[A]()

    request.onsuccess = { event =>
      promise.success(success(event))
    }

    request.onerror = { event =>
      promise.failure(error(event))
    }

    promise.future
  }

  override def getAll[A](): Future[Seq[ModelStore]] = {
    // TODO: Complete cursor to get model store
    for {
      db <- getDatabase()
    } yield {
     List()
    }
  }

  override def persist(model: ModelStore): Future[Boolean] = {
    Future(false)
  }

  override def delete(model: ModelStore): Future[Boolean] = {
    Future(false)
  }
}
