package rlp.dao

import rlp.storage.ModelStore

import scala.concurrent.Future

object ServerModelDAO extends ModelDAO {

  override def getAll(): Future[Seq[ModelStore]] = ???

  override def create(model: ModelStore): Future[Long] = ???

  override def update(model: ModelStore): Future[Unit] = ???

  override def delete(id: Long): Future[Unit] = ???
}
