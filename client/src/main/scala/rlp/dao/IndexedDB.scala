package rlp.dao

import org.scalajs.dom.idb.{Database, ObjectStore, Request}
import org.scalajs.dom._
import rlp.utils.Logger

import scala.concurrent.{Future, Promise}
import scala.scalajs.js
import scala.concurrent.ExecutionContext.Implicits.global

object IndexedDB {

  val DB_NAME = "rlp"
  val MODEL_STORE = "modelStore"

  def getDatabase(): Future[Database] = {

    val dBRequest = window.indexedDB.open(DB_NAME, 1)

    dBRequest.onupgradeneeded = { _ =>
      Logger.log("IndexedDB", s"Upgrading database with $MODEL_STORE database")

      val db = dBRequest.result.asInstanceOf[Database]
      db.createObjectStore(MODEL_STORE, js.Dynamic.literal("keyPath" -> "id"))
    }

    requestPromise[Database](dBRequest) { _ =>
      dBRequest.result.asInstanceOf[Database]
    }
  }

  def objectStore(storeName: String, readOnly: Boolean = true): Future[ObjectStore] = {
    getDatabase() map { db =>
      db.transaction(storeName, if (readOnly) "readonly" else "readwrite")
        .objectStore(storeName)
    }
  }

  def create(storeName: String, item: js.Any): Future[Unit] = {
    objectStore(storeName, readOnly = false) flatMap { store =>
      requestPromise(store.add(item))
    }
  }

  def retrieve[A](storeName: String, key: js.Any): Future[A] = {
    objectStore(storeName) flatMap { store =>
      val request: Request = store.get(key)

      requestPromise[A](request) { _ =>
        request.result.asInstanceOf[A]
      }
    }
  }

  def update(storeName: String, item: js.Any): Future[Unit] = {
    objectStore(storeName, readOnly = false) flatMap { store =>
      requestPromise(store.put(item))
    }
  }

  def delete(storeName: String, key: js.Any): Future[Unit] = {
    objectStore(storeName, readOnly = false) flatMap { store =>
      requestPromise(store.delete(key))
    }
  }

  def getAll[A](storeName: String): Future[Seq[A]] = {
    objectStore(storeName) flatMap { store =>

      val buffer = js.Array[A]()
      val promise = Promise[Seq[A]]()
      val cursor = store.openCursor()

      cursor.onerror = { _ =>
        Logger.log("IndexedDB", "Cursor get failed: " + cursor.error.toString)
        promise.failure(new Exception(cursor.error.toString))
      }

      cursor.onsuccess = { event =>
        val cursor = event.target.asInstanceOf[js.Dynamic].result
        if (cursor != null) {
          buffer.append(cursor.value.asInstanceOf[A])
          cursor.continue()
        } else {
          promise.success(buffer)
        }
      }

      promise.future
    }
  }

  def requestPromise(request: Request): Future[Unit] = {
    requestPromise[Unit](request) { _ => () }
  }

  def requestPromise[A](request: Request)(success: Event => A): Future[A] = {
    requestPromise(
      request,
      success,
      err => {
        Logger.log("IndexedDB", "Error: " + err.toString)
        new Exception(err.toString)
      }
    )
  }

  def requestPromise[A](request: Request, success: Event => A, error: DOMError => Throwable): Future[A] = {
    val promise = Promise[A]()

    request.onsuccess = { event =>
      promise.success(success(event))
    }

    request.onerror = { _ =>
      promise.failure(error(request.error))
    }

    promise.future
  }
}
