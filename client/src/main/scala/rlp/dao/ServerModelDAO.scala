package rlp.dao

import rlp.storage.ModelStore

import scala.concurrent.Future

object ServerModelDAO extends ModelDAO {

  override def getAll[A](): Future[Seq[ModelStore]] = ???

  override def update(model: ModelStore): Future[Boolean] = ???

  override def delete(model: ModelStore): Future[Boolean] = ???
}
