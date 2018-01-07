package rlp.dao
import rlp.models.Model

object LocalModelStore extends ModelDAO {

  override def getAll[A](): Model[A] = ???

  override def persist(model: Model[_]): Unit = ???
}
