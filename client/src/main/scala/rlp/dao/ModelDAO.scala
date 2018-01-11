package rlp.dao

import rlp.models.Model

trait ModelDAO {

  def getAll[A](): Seq[Model[A]]

  def persist(model: Model[_]): Boolean

  def delete(model: Model[_]): Boolean
}
