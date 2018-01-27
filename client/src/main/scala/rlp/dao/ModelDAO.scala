package rlp.dao

import rlp.models.ModelStore

import scala.concurrent.Future

trait ModelDAO {

  def getAll(): Future[Seq[ModelStore]]

  def create(model: ModelStore): Future[Long]

  def update(model: ModelStore): Future[Unit]

  def delete(id: Long): Future[Unit]
}
