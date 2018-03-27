package rlp.storage

import java.nio.ByteBuffer

/**
  * Base64 compression algorithm for encoding/decoding binary data
  */
object Base64 {

  val base64 = Seq('A' to 'Z', 'a' to 'z', '0' to '9', Seq('+','/')).reduce(_ ++ _).toArray

  val invBase64 = {
    val arr = new Array[Int](256)
    for ((b, i) <- base64.zipWithIndex) arr(b.toInt) = i
    arr
  }

  def toByteArray(arr: Array[Double]): Array[Byte] = {
    val buffer = ByteBuffer.allocate(arr.length * 8)
    arr foreach { a => buffer.putDouble(a) }
    buffer.array()
  }

  def fromByteArray(arr: Array[Byte]): Array[Double] = {
    val buffer = ByteBuffer.wrap(arr).asDoubleBuffer()
    val doubleArr = new Array[Double](buffer.limit())
    buffer.get(doubleArr)
    doubleArr
  }

  def encode(arr: Array[Byte]): String = {
    val padding = (3 - arr.length % 3) % 3
    val builder = new StringBuilder(4*(arr.length + padding)/3 + 1)

    def getItem(index: Int): Int = if (index < arr.length) arr(index) & 0xFF else 0

    for (i <- 0 until (arr.length + padding) by 3) {
      val z = (getItem(i) << 16) | (getItem(i+1) << 8) | getItem(i+2)
      builder += base64((z >> 18) & 0x3f)
      builder += base64((z >> 12) & 0x3f)
      builder += base64((z >> 6) & 0x3f)
      builder += base64(z & 0x3f)
    }

    builder += "012"(padding)

    builder.toString
  }

  def decode(s: String): Array[Byte] = {

    require(s.length % 4 == 1, "Invalid base64 string")

    val padding = s.last - '0'
    val arr = new Array[Byte](3 * (s.length - 1) / 4 - padding)

    for (i <- 0 until (s.length - 1) by 4) {

      val z = (invBase64(s(i)) << 18) | (invBase64(s(i+1)) << 12) | (invBase64(s(i+2)) << 6) | invBase64(s(i+3))

      val x = 3*(i/4)
      arr(x) = (z >> 16).toByte
      if (x+1 < arr.length) arr(x+1) = (z >> 8).toByte
      if (x+2 < arr.length) arr(x+2) = z.toByte
    }

    arr
  }

}
