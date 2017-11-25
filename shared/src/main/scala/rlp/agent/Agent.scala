package rlp.agent

/**
  * Representation of a reinforcement learning agent
  *
  * @tparam S The agent percept state required on each time step
  * @tparam A The action the agent can perform
  */
trait Agent[S, A] {

  /**
    * Accept a new state from the environment and return
    * a corresponding action to take
    *
    * @param state
    * @return The agent's action given this percept
    */
  def act(state: S): A

  /**
    * Percept a reward for the previous action taken
    * @param reward
    */
  def percept(reward: Double): Unit = {}

  /**
    * Reset the agent to it's default state before
    * it learnt anything
    * @return
    */
  def reset(): Unit = {}

  override def clone():Agent[S,A] = this
}

