package rlp.storage

import upickle.Js

trait Storable {

  def store(): Js.Value

  def load(json: Js.Value): Unit
}

object Storable {

  def binaryStore(data: Array[Double]): Js.Str = {
    Js.Str(Base64.encode(Base64.toByteArray(data)))
  }

  def binaryRead(str: String): Array[Double] = {
    Base64.fromByteArray(Base64.decode(str))
  }

  def store(storable: Storable): Js.Value = {
    storable.store()
  }

  def store[A <: Storable](xs: Seq[A]): Js.Arr = {
    Js.Arr(xs map (_.store()) :_*)
  }

  def load[A <: Storable](xs: Array[A], json: Js.Value): Unit = {
    val items = json.arr
    for (i <- xs.indices) xs(i).load(items(i))
  }

}
