package rlp.agent
import ujson.Js

/**
  * Convenient abstraction on top of a regular Agent
  * for agents which combine acting and perception into
  * one step
  *
  * @tparam S The percept state
  * @tparam A Agent action
  */
trait SteppedAgent[S, A] extends Agent[S, A] {

  /* Internal state to keep track of actions and rewards taken */
  private var lastReward: Double = 0
  private var lastAction: A = _
  private var lastState: S = _
  private var first = true

  override def act(state: S): A = {
    lastAction = step(lastState, lastAction, lastReward, state, first, false)
    first = false
    lastState = state
    lastAction
  }

  override def percept(reward: Double): Unit = {
    lastReward = reward
  }

  override def resetEpisode(): Unit = {
    if (!first) {
      step(lastState, lastAction, lastReward, lastState, first, true)
    }
    first = true
  }

  def step(prevState: S, action: A, reward: Double, newState: S, first: Boolean, last: Boolean): A

  /**
    * Clone the agent
    *
    * @return A new agent with new internal state but the same step function
    */
  override def clone(): SteppedAgent[S, A] = {

    val that = this

    new SteppedAgent[S,A] {

      override def step(prevState: S, action: A, reward: Double, newState: S, first: Boolean, last: Boolean): A = {
        that.step(prevState, action, reward, newState, first, last)
      }

      override def load(data: Js.Value): Unit = that.load(data)

      override def store(): Js.Value = that.store()
    }
  }
}
