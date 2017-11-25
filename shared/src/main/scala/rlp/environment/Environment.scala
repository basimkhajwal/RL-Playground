package rlp.environment

/**
  * Represents an environment in reinforcement learning
  * that has an internal state totally defined by S. This
  * is mutated on each time step based on actions and the
  * dynamics of the environment
  *
  * @tparam S The internal state of the environment
  */
trait Environment[S] {

  /**
    * Generate a random internal state
    * @param seed The seed for pseudo-random numbers
    */
  def reset(seed: Int = (math.random()*10000000).toInt): Unit

  /**
    * Change the internal state to the one applied
    * @param state New internal state
    */
  def setState(state: S): Unit

  /**
    * @return Current internal state
    */
  def getState(): S

  /**
    * Mutate state
    * @return Whether the environment has reached a terminal state (reset required)
    */
  def step(): Boolean
}

object Environment {
  /* Default time period in seconds between environment steps */
  val FPS = 30
  val DELTA = 1.0/FPS
}