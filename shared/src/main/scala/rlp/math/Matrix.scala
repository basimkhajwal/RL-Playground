package rlp.math

class Matrix(private var rows: Int, private var cols: Int, private var data: Array[Double]) {

  def this(rows: Int, cols: Int) = {
    this(rows, cols, new Array[Double](rows * cols))
  }

  def this(that: Matrix) = {
    this(that.rows, that.cols, that.data.clone())
  }

  @inline
  def getRows() = rows
  @inline
  def getCols() = cols

  @inline
  def apply(row: Int, col: Int): Double = {
    assert(0 <= row && 0 <= col && row < rows && col < cols, s"Index $row $col does not exist")
    data(row*cols + col)
  }

  @inline
  def update(row: Int, col: Int, value: Double): Unit = {
    assert(0 <= row && 0 <= col && row < rows && col < cols, s"Index $row $col does not exist")
    data(row*cols + col) = value
  }

  def +(that: Matrix): Matrix = new Matrix(this) += that

  def +=(that: Matrix): Matrix = {
    assert(rows == that.rows && cols == that.cols,
      s"Matrix addition not defined for ${rows}x$cols and ${that.rows}x${that.cols}")
    var i = 0
    while (i < data.length) {
      data(i) += that.data(i)
      i += 1
    }
    this
  }

  def *(sf: Double): Matrix = new Matrix(this) *= sf

  def *(that: Matrix): Matrix = new Matrix(this) *= that

  def *=(sf: Double): Matrix = {
    var i = 0
    while (i < data.length) {
      data(i) *= sf
      i += 1
    }
    this
  }

  def *=(that: Matrix): Matrix = {
    assert(cols == that.rows,
      s"Matrix addition not defined for ${rows}x$cols and ${that.rows}x${that.cols}")

    val buffer = new Array[Double](rows * that.cols)
    var i, j, k = 0

    while (i < rows) {
      j = 0
      while (j < that.cols) {
        k = 0
        while (k < cols) {
          buffer(i*that.cols + j) += this(i, k) * that(k, j)
          k += 1
        }
        j += 1
      }
      i += 1
    }

    cols = that.cols
    data = buffer
    this
  }
}
