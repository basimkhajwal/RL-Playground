package rlp.utils

import com.thoughtworks.binding.Binding.{Var, Vars}

import scala.collection.mutable.ArrayBuffer

class HistoryBuffer(val maxSize: Int = 1000) {

  val historyStep: Var[Int] = Var(1)
  val history: Vars[Double] = Vars()

  private var recentHistoryTotal: Double = 0
  private var recentHistoryCount: Int = 0

  def clear(): Unit = {
    historyStep := 1
    history.get.clear()
    recentHistoryCount = 0
    recentHistoryTotal = 0
  }

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
    recentHistoryTotal += data
    recentHistoryCount += 1

    if (recentHistoryCount == historyStep.get) {
      history.get += recentHistoryTotal / historyStep.get

      recentHistoryTotal = 0
      recentHistoryCount = 0

      if (history.get.length >= maxSize) {
        collateHistory()
      }
    }
  }
}
