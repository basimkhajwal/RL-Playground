package rlp.math

import rlp.storage.Storable
import ujson.Js

/**
  * Storing and applying matrix operations to the data supplied
  *
  * @param rows
  * @param cols
  * @param data The matrix values, in row-major order
  */
class Matrix(var rows: Int, var cols: Int, var data: Array[Double]) extends Storable {

  require(rows > 0)
  require(cols > 0)
  require(data.length == rows*cols, "Incorrect data supplied")

  for (i <- data.indices) {
    if (data(i).isNaN) throw new Exception("Invalid NAN!")
  }

  def this(rows: Int, cols: Int) = {
    this(rows, cols, new Array[Double](rows * cols))
  }

  def this(that: Matrix) = {
    this(that.rows, that.cols, that.data.clone())
  }

  def fillWith(v: Double): Matrix = {
    for (i <- 0 until (rows*cols)) data(i) = v
    this
  }

  def subMatrix(rowStart: Int = 0, rowEnd: Int = rows, colStart: Int = 0, colEnd: Int = cols): Matrix = {
    val ret = new Matrix(rowEnd-rowStart, colEnd-colStart)
    for (r <- rowStart until rowEnd) {
      for (c <- colStart until colEnd) {
        ret(r - rowStart, c - colStart) = this(r, c)
      }
    }
    ret
  }

  def transpose(): Matrix = new Matrix(this) transposeSelf()

  def transposeSelf(): Matrix = {

    val buffer = new Array[Double](data.length)

    for (r <- 0 until rows) {
      for (c <- 0 until cols) {
        buffer(c*rows+r) = data(r*cols+c)
      }
    }

    data = buffer
    val temp = rows
    rows = cols
    cols = temp
    this
  }

  def map(f: Double => Double): Matrix = {
    new Matrix(this) each f
  }

  def each(f: Double => Double): Matrix = {
    for (i <- 0 until (rows*cols)) data(i) = f(data(i))
    this
  }

  def apply(index: Int): Double = {
    require(index < data.length, "Invalid index")
    data(index)
  }

  def apply(row: Int, col: Int): Double = {
    require(0 <= row && 0 <= col && row < rows && col < cols, s"Index $row $col does not exist")
    data(row*cols + col)
  }

  def update(index: Int, value: Double): Unit = {
    require(index < data.length, "Invalid index")
    require(!value.isNaN, "Value is NaN")
    data(index) = value
  }

  def update(row: Int, col: Int, value: Double): Unit = {
    require(0 <= row && 0 <= col && row < rows && col < cols, s"Index $row $col does not exist")
    require(!value.isNaN, "Value is NaN")
    data(row*cols + col) = value
  }

  def elemProduct(that: Matrix): Matrix = {
    new Matrix(this) elemProductSelf that
  }

  def elemProductSelf(that: Matrix): Matrix = {
    require(rows == that.rows && cols == that.cols,
      "Element-wise matrix multiplication only defined for same sizes!")
    for (r <- 0 until rows) {
      for (c <- 0 until cols) {
        this(r, c) *= that(r, c)
      }
    }
    this
  }

  def +(that: Matrix): Matrix = {
    new Matrix(this) += that
  }

  def +=(that: Matrix): Matrix = {
    require(rows == that.rows && cols == that.cols,
      s"Matrix addition not defined for ${rows}x$cols and ${that.rows}x${that.cols}")
    for (i <- 0 until (rows*cols)) data(i) += that.data(i)
    this
  }

  def -(that: Matrix): Matrix = new Matrix(this) -= that

  def -=(that: Matrix): Matrix = {
    require(rows == that.rows && cols == that.cols,
      s"Matrix subtraction not defined for ${rows}x$cols and ${that.rows}x${that.cols}")
    for (i <- 0 until (rows*cols)) data(i) -= that.data(i)
    this
  }

  def *(sf: Double): Matrix = {
    new Matrix(this) *= sf
  }

  def *(that: Matrix): Matrix = {
    new Matrix(this) *= that
  }

  def *=(sf: Double): Matrix = {
    for (i <- 0 until (rows*cols)) data(i) *= sf
    this
  }

  def *=(that: Matrix): Matrix = {
    require(cols == that.rows,
      s"Matrix multiplication not defined for ${rows}x$cols and ${that.rows}x${that.cols}")

    val buffer = new Array[Double](rows * that.cols)
    for (i <- 0 until rows) {
      for (j <- 0 until that.cols) {
        for (k <- 0 until cols) {
          buffer(i*that.cols + j) += this(i, k) * that(k, j)
        }
      }
    }

    cols = that.cols
    data = buffer
    this
  }

  def store(): Js.Value = {
    Storable.binaryStore(data)
  }

  def load(json: Js.Value): Unit = {
    val xs = Storable.binaryRead(json.str.toString)
    require(xs.length == data.length, s"Cannot load $rows x $cols matrix from ${xs.length} values")
    data = xs
  }

  override def clone(): Matrix = {
    new Matrix(this)
  }

  override def toString: String = {
    var str = "["
    for (r <- 0 until rows) {
      if (r != 0) str += ",\n "
      for (c <- 0 until cols) {
        if (c != 0) str += " "
        str += this(r, c).formatted("%.5f")
      }
    }
    str + " ]"
  }
}

object Matrix {

  /**
    * Create a new matrix representing row vectors
    * with the data given
    * @param data
    * @return
    */
  def rows(data: Array[Array[Double]]): Matrix = {
    new Matrix(data.length, data(0).length, data.flatten)
  }

  def rows(data: Array[Double]*): Matrix = rows(data.toArray)

  /**
    * Concatenate matrices supplied along the row dimension
    * @param ms
    * @return
    */
  def concatRows(ms: Matrix*): Matrix = {
    require(ms.nonEmpty, "Cannot concat empty list of matrices!")
    require(ms.map(_.cols).distinct.length == 1,
      s"Concatenating along rows requires the same size on cols")

    val ret = new Matrix(ms.map(_.rows).sum, ms.head.cols)
    var r = 0

    for (m <- ms) {
      Array.copy(m.data, 0, ret.data, r*ret.cols, m.data.size)
      r += m.rows
    }

    ret
  }

  /**
    * Concatenate matrices supplied along the column dimension
    * @param ms
    * @return
    */
  def concatCols(ms: Matrix*): Matrix = {
    require(ms.nonEmpty, "Cannot concat empty list of matrices!")
    require(ms.map(_.rows).distinct.length == 1,
      s"Concatenating along cols requires the same size on rows")

    val ret = new Matrix(ms.head.rows, ms.map(_.cols).sum)

    var cTotal = 0
    for (m <- ms) {
      for (r <- 0 until m.rows) {
        for (c <- 0 until m.cols) {
          ret(r, cTotal+c) = m(r, c)
        }
      }
      cTotal += m.cols
    }

    ret
  }

  /**
    * Apply softmax across the rows of this matrix
    *
    * @param m
    * @return Softmax matrix
    */
  def softMax(m: Matrix): Matrix = {
    val res = new Matrix(m.rows, m.cols)

    for (r <- 0 until m.rows) {

      // Extract the maximum element before using math.exp for better numerical stability
      var maxElem = 0.0
      for (c <- 0 until m.cols) maxElem = math.max(maxElem, m(r,c))

      var total = 0.0
      for (c <- 0 until m.cols) total += math.exp(m(r, c) - maxElem)

      for (c <- 0 until m.cols) {
        res(r,c) = math.exp(m(r, c) - maxElem) / total
      }
    }

    res
  }
}