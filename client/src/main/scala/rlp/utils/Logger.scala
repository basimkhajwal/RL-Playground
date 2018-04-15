package rlp.utils

/**
  * Simple logger to print messages, centralising
  * debug messages makes it easy to disable in production
  */
object Logger {

  def log(msg: String): Unit = {
    println(msg)
  }

  def log(name: String, msg: String): Unit = log(name + ":\t" + msg)
}
