package rlp.math

/**
  * Functions on arrays of doubles dealing
  * with distributions of data
  */
object Distribution {

  def mean(values: Array[Double]): Double = {
    if (values.length > 0) {
      values.sum / values.length
    } else {
      0
    }
  }

  def stdDev(values: Array[Double]): Double = {

    if (values.length < 2) return 0

    val n = values.length

    var sum = 0.0
    var sumSquared = 0.0
    for (value <- values) {
      sum += value
      sumSquared += value*value
    }

    math.sqrt((sumSquared - sum*sum/n) / (n-1))
  }
}
