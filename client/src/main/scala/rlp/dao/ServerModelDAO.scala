package rlp.dao

import rlp.storage.ModelStore

import scala.concurrent.Future

object ServerModelDAO extends ModelDAO {

  override def getAll[A](): Future[Seq[ModelStore]] = ???

  override def create(model: ModelStore): Future[Long] = ???

  override def update(model: ModelStore): Future[Unit] = ???

  override def delete(model: ModelStore): Future[Unit] = ???
}
