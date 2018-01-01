package rlp.storage

import java.nio.ByteBuffer

object Base64 {

  val base64 = Seq('A' to 'Z', 'a' to 'z', '0' to '9', Seq('+','/')).reduce(_ ++ _).toArray

  def toByteArray(arr: Array[Double]): Array[Byte] = {
    val buffer = ByteBuffer.allocate(arr.length * 8)
    arr foreach { a => buffer.putDouble(a) }
    buffer.array()
  }

  def fromByteArray(arr: Array[Byte]): Array[Double] = {
    ByteBuffer.wrap(arr).asDoubleBuffer().array()
  }

  def encode(arr: Array[Byte]): String = {
    val padding = (3 - arr.length % 3) % 3
    val builder = new StringBuilder(4*(arr.length + padding)/3 + 1)



    builder(builder.length - 1) = "012"(padding)

    builder.toString
  }

  def decode(s: String): Array[Byte] = {

    ???
  }



}
