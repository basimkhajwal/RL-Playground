package rlp.storage

import ujson.Js

/**
  * Storable interface for classes
  * which have mutable internal state
  */
trait Storable {

  /**
    * Store the internal state into a JSON object
    * @return
    */
  def store(): Js.Value

  /**
    * Extract the JSON object to set the internal
    * state
    *
    * @param json
    */
  def load(json: Js.Value): Unit
}

object Storable {

  /* Use of Base 64 to encode/decode double arrays */

  def binaryStore(data: Array[Double]): Js.Str = {
    Js.Str(Base64.encode(Base64.toByteArray(data)))
  }

  def binaryRead(str: String): Array[Double] = {
    Base64.fromByteArray(Base64.decode(str))
  }

  /* Functions for dealing with collections of Storable items */

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
