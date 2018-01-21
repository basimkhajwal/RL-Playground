package rlp.utils

import org.scalajs.dom.window.performance

import scala.scalajs.js.timers

/**
  * Runs an operation that needs to be run continually in the background
  * without blocking the UI thread (at least not for large intervals of time)
  *
  * @param processStep The process to run
  * @param name Optional, name for debugging
  * @param benchmarkRuns The runs used when benchmarking (to optimise performance)
  */
class BackgroundProcess(
  processStep: () => Unit,
  name: String = "",
  benchmarkRuns: Int = 1000
) {

  private var isRunning: Boolean = false
  private var timerHandle: timers.SetIntervalHandle = _

  /** Estimate time per process step
    *
    * @return Process average time (milliseconds)
    */
  private def runBenchmark(): Double = {
    val start = performance.now()
    for (_ <- 0 until benchmarkRuns) processStep()
    val end = performance.now()

    (end - start) / benchmarkRuns
  }

  /**
    * Start the process (non-blocking)
    *
    * @param targetStepRate The target steps per second
    * @param intervalDelta If the target exceeds browser refresh rate or is negative
    */
  def start(targetStepRate: Int, intervalDelta: Double = 10): Unit = {

    if (targetStepRate < 0 || targetStepRate > 200) {

      val trainTime = runBenchmark()
      val stepTicks = ((intervalDelta-1) / trainTime).toInt

      timerHandle = timers.setInterval(intervalDelta) {
        for (_ <- 0 until stepTicks) processStep()
      }

      if (name != "") {
        Logger.log(s"Process $name", s"Benchmarked at ${trainTime}ms per step")
        Logger.log(s"Process $name", s"Started at ${stepTicks * 1000 / intervalDelta}fps")
      }

    } else {
      timerHandle = timers.setInterval(1000.0 / targetStepRate) {
        processStep()
      }

      if (name != "") {
        Logger.log(s"Process $name", s"Started at ${targetStepRate}fps")
      }
    }
    isRunning = true
  }

  def running(): Boolean = isRunning

  /**
    * Stop the current process (throws an error if process not started)
    */
  def stop(): Unit = {
    assert(isRunning, "Stopping process that wasn't started")

    if (name != "") Logger.log(s"Process $name", "Stopped")

    timers.clearInterval(timerHandle)
    isRunning = false
  }

}

object BackgroundProcess {

  /**
    * Utility function to instantiate a background process with a more convenient
    * syntax for block expressions
    *
    * @param name
    * @param benchmarkRuns
    * @param step
    * @return
    */
  def apply(name: String = "", benchmarkRuns: Int = 1000)(step: () => Unit): BackgroundProcess = {
    new BackgroundProcess(step, name, benchmarkRuns)
  }
}
