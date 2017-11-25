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

  override def act(state: S): A = {
    lastAction = step(lastState, lastAction, lastReward, state)
    lastState = state
    lastAction
  }

  override def percept(reward: Double): Unit = {
    lastReward = reward
  }

  /**
    * Combined act and percept functions into a
    * single step
    *
    * @param prevState
    * @param action
    * @param reward
    * @param newState
    * @return
    */
  def step(prevState: S, action: A, reward: Double, newState: S): A

  /**
    * Clone the agent
    *
    * @return A new agent with new internal state but the same step function
    */
  override def clone(): SteppedAgent[S, A] = step
}
