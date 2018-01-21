package rlp.dao

import org.scalajs.dom.ext.Ajax
import rlp.storage.ModelStore

import scala.concurrent.Future
import upickle.default

import scala.util.Try

object ServerModelDAO extends ModelDAO {

  override def getAll(): Future[Seq[ModelStore]] = {
    Ajax.get("/models") flatMap { request =>
      Future.fromTry(Try {
        default.read[Seq[ModelStore]](request.responseText)
      })
    }
  }

  override def create(model: ModelStore): Future[Long] = {
    Ajax.put("/models", default.write(model)) flatMap { request =>
      Future.fromTry(Try(
        request.getResponseHeader("id").toLong
      ))
    }
  }

  override def update(model: ModelStore): Future[Unit] = {
    Ajax.post("/models", default.write(model)).map { _ => () }
  }

  override def delete(id: Long): Future[Unit] = {
    Ajax.delete("/models", headers = Map("id" -> id.toString)) map { _ => () }
  }
}
