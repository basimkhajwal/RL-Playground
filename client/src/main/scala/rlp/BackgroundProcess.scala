package rlp

import org.scalajs.dom.window.performance

import scala.scalajs.js.timers

class BackgroundProcess(
  processStep: () => Unit,
  name: String = "",
  benchmarkRuns: Int = 1000
) {

  // TODO: Add comments and logging

  private var isRunning: Boolean = false
  private var timerHandle: timers.SetIntervalHandle = _

  private def runBenchmark(): Double = {
    val start = performance.now()
    for (_ <- 0 until benchmarkRuns) processStep()
    val end = performance.now()

    (end - start) / benchmarkRuns
  }

  def start(targetStepRate: Int, intervalDelta: Double = 10): Unit = {

    if (targetStepRate < 0 || targetStepRate > 200) {

      val trainTime = runBenchmark()
      val stepTicks = ((intervalDelta-1) / trainTime).toInt

      timerHandle = timers.setInterval(intervalDelta) {
        for (_ <- 0 until stepTicks) processStep()
      }

    } else {
      timerHandle = timers.setInterval(1000.0 / targetStepRate) {
        processStep()
      }
    }

    isRunning = true
  }

  def running(): Boolean = isRunning

  def stop(): Unit = {
    assert(isRunning, "Stopping process that wasn't started")

    timers.clearInterval(timerHandle)
    isRunning = false
  }

}
