package rlp.dao

import rlp.storage.ModelStore

import scala.concurrent.Future

trait ModelDAO {

  def getAll[A](): Future[Seq[ModelStore]]

  def persist(model: ModelStore): Future[Boolean]

  def delete(model: ModelStore): Future[Boolean]
}
