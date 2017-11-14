package rlp

object Logger {

  def log(msg: String): Unit = {
    println(msg)
  }

  def log(name: String, msg: String): Unit = log(name + ":\t" + msg)
}
