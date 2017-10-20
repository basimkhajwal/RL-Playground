package rlp.math

class Matrix(private var rows: Int, private var cols: Int, private var data: Array[Double]) {

  def this(rows: Int, cols: Int) = {
    this(rows, cols, new Array[Double](rows * cols))
  }

  def this(that: Matrix) = {
    this(that.rows, that.cols, that.data.clone())
  }

  def getRows() = rows
  def getCols() = cols
  def getData() = data

  def fill(v: Double): Matrix = {
    for (i <- data.indices) data(i) = v
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

  def map(f: Double => Double): Matrix = new Matrix(this) each f

  def each(f: Double => Double): Matrix = {
    for (i <- data.indices) data(i) = f(data(i))
    this
  }

  def apply(row: Int, col: Int): Double = {
    assert(0 <= row && 0 <= col && row < rows && col < cols, s"Index $row $col does not exist")
    data(row*cols + col)
  }

  def update(row: Int, col: Int, value: Double): Unit = {
    assert(0 <= row && 0 <= col && row < rows && col < cols, s"Index $row $col does not exist")
    data(row*cols + col) = value
  }

  def elemProduct(that: Matrix): Matrix = new Matrix(this) *= that

  def elemProductSelf(that: Matrix): Matrix = {
    assert(rows == that.rows && cols == that.cols, "Element-wise matrix multiplication only defined for same sizes!")
    for (r <- 0 until rows) {
      for (c <- 0 until cols) {
        this(r, c) *= that(r, c)
      }
    }
    this
  }

  def +(that: Matrix): Matrix = new Matrix(this) += that

  def +=(that: Matrix): Matrix = {
    assert(rows == that.rows && cols == that.cols,
      s"Matrix addition not defined for ${rows}x$cols and ${that.rows}x${that.cols}")
    for (i <- data.indices) data(i) += that.data(i)
    this
  }

  def unary_-(): Matrix = new Matrix(this) *= (-1)

  def -(that: Matrix): Matrix = new Matrix(this) -= that

  def -=(that: Matrix): Matrix = {
    assert(rows == that.rows && cols == that.cols,
      s"Matrix subtraction not defined for ${rows}x$cols and ${that.rows}x${that.cols}")
    for (i <- data.indices) data(i) -= that.data(i)
    this
  }

  def *(sf: Double): Matrix = new Matrix(this) *= sf

  def *(that: Matrix): Matrix = new Matrix(this) *= that

  def *=(sf: Double): Matrix = {
    for (i <- data.indices) data(i) *= sf
    this
  }

  def *=(that: Matrix): Matrix = {
    assert(cols == that.rows,
      s"Matrix addition not defined for ${rows}x$cols and ${that.rows}x${that.cols}")

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

  override def toString: String = {
    var str = "[ "
    for (r <- 0 until rows) {
      if (r != 0) str += ", "
      for (c <- 0 until cols) {
        if (c != 0) str += " "
        str += this(r, c)
      }
    }
    str + " ]"
  }
}

object Matrix {

  def concatRows(ms: Matrix*): Matrix = {
    assert(ms.nonEmpty, "Cannot concat empty list of matrices!")
    assert(ms.map(_.cols).distinct.length == 1,
      s"Concatenating along rows requires the same size on cols")

    val ret = new Matrix(ms.map(_.rows).sum, ms.head.cols)
    var r = 0

    for (m <- ms) {
      Array.copy(m.data, 0, ret, r*ret.cols, m.data.size)
      r += m.rows
    }

    ret
  }

  def concatCols(ms: Matrix*): Matrix = {
    assert(ms.nonEmpty, "Cannot concat empty list of matrices!")
    assert(ms.map(_.rows).distinct.length == 1,
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
}