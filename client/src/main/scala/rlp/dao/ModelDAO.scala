package rlp.dao

import rlp.models.Model

trait ModelDAO {

  def getAll[A](): Model[A]

  def persist(model: Model[_])
}
