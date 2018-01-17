package rlp.dao

import rlp.storage.ModelStore

import scala.concurrent.Future

trait ModelDAO {

  def getAll(): Future[Seq[ModelStore]]

  def create(model: ModelStore): Future[Long]

  def update(model: ModelStore): Future[Unit]

  def delete(model: ModelStore): Future[Unit]
}
