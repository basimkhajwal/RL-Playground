package rlp.agent

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

  override def percept(reward: Double, done: Boolean): Unit = {
    lastReward = reward
    if (done) {
      step(lastState, lastAction, lastReward, lastState, first, true)
      first = true
    }
  }

  def step(prevState: S, action: A, reward: Double, newState: S, first: Boolean, last: Boolean): A

  /**
    * Clone the agent
    *
    * @return A new agent with new internal state but the same step function
    */
  override def clone(): SteppedAgent[S, A] = step
}
