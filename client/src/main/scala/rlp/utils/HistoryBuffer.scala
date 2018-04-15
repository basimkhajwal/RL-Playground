package rlp.utils

import com.thoughtworks.binding.Binding.{Var, Vars}

import scala.collection.mutable.ArrayBuffer

/**
  * A buffer which records time series values up to a maximum,
  * when overflowing it averages out adjacent values to reduce
  * memory consumption
  *
  * @param maxSize
  */
class HistoryBuffer(val maxSize: Int = 1000) {

  /**
    * The step between adjacent values in terms of the initial amount
   */
  val historyStep: Var[Int] = Var(1)

  /**
    * The history values themselves
    */
  val history: Vars[Double] = Vars()

  private var recentHistoryTotal: Double = 0
  private var recentHistoryCount: Int = 0

  /**
    * Reset all the history
    */
  def clear(): Unit = {
    historyStep := 1
    history.get.clear()
    recentHistoryCount = 0
    recentHistoryTotal = 0
  }

  /**
    * Average out pairs of values to reduce them down to one
    */
  private def collateHistory(): Unit = {
    val currentHistory = history.get
    val newHistory = ArrayBuffer[Double]()

    newHistory += currentHistory(0)
    for (i <- 2 until currentHistory.length by 2) {
      newHistory += (currentHistory(i) + currentHistory(i-1)) / 2
    }

    historyStep := historyStep.get * 2
    history.get.clear()
    history.get.appendAll(newHistory)
  }

  final def add(data: Double): Unit = {

    // Add the data
    recentHistoryTotal += data
    recentHistoryCount += 1

    // Check for step increase
    if (recentHistoryCount == historyStep.get) {

      history.get += recentHistoryTotal / historyStep.get

      recentHistoryTotal = 0
      recentHistoryCount = 0

      // Collate to reduce size
      if (history.get.length >= maxSize) {
        collateHistory()
      }
    }
  }
}
