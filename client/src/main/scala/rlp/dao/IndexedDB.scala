package rlp.dao

import org.scalajs.dom.idb.{Database, ObjectStore, Request}
import org.scalajs.dom._

import scala.concurrent.{Future, Promise}
import scala.scalajs.js
import scala.concurrent.ExecutionContext.Implicits.global

object IndexedDB {

  val DB_NAME = "rlp"
  val MODEL_STORE = "modelStore"
  val FAIL_SAFE = "failSafe"

  def getDatabase(): Future[Database] = {

    val dBRequest = window.indexedDB.open(DB_NAME, 1)

    dBRequest.onupgradeneeded = { event =>
      val db = dBRequest.result.asInstanceOf[Database]

      val modelStore = db.createObjectStore(MODEL_STORE, js.Dynamic.literal("keyPath" -> "id"))

      val failSafe = db.createObjectStore(FAIL_SAFE, js.Dynamic.literal("keyPath" -> "id"))
      failSafe.createIndex("uid", "uid", js.Dynamic.literal("unique" -> false))
    }

    requestPromise(dBRequest) { _ =>
      dBRequest.result.asInstanceOf[Database]
    }
  }

  def objectStore(storeName: String, readOnly: Boolean = true): Future[ObjectStore] = {
    getDatabase() map { db =>
      db.transaction(storeName, if (readOnly) "readonly" else "readwrite").objectStore(storeName)
    }
  }

  def create(storeName: String, item: js.Any): Future[Unit] = {
    objectStore(storeName, readOnly = false) flatMap { store =>
      requestPromise(store.add(item)) { _ => () }
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
      requestPromise(store.put(item)) { _ => () }
    }
  }

  def delete(storeName: String, key: js.Any): Future[Unit] = {
    objectStore(storeName, readOnly = false) flatMap { store =>
      requestPromise(store.delete(key)) { _ => () }
    }
  }

  def getAll[A](storeName: String): Future[Seq[A]] = {
    objectStore(storeName) flatMap { store =>

      val buffer = js.Array[A]()
      val promise = Promise[Seq[A]]()
      val cursor = store.openCursor()

      cursor.onerror = { e:ErrorEvent =>
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

  def requestPromise[A](request: Request)(success: Event => A): Future[A] = {
    requestPromise(request, success, err => new Exception(err.toString))
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
