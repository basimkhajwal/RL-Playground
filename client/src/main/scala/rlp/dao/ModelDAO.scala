package rlp.dao


import rlp.presenters.AgentStore

import scala.concurrent.Future

trait ModelDAO {

  def getAll(): Future[Seq[AgentStore]]

  def create(model: AgentStore): Future[Long]

  def update(model: AgentStore): Future[Unit]

  def delete(id: Long): Future[Unit]
}
