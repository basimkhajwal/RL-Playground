package rlp.dao
import rlp.models.Model

object LocalModelStore extends ModelDAO {

  override def getAll[A](): Seq[Model[A]] = ???

  override def persist(model: Model[_]): Boolean = ???

  override def delete(model: Model[_]) = ???
}
