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
  val DELTA = 1.0/30
}

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

  override def clone():Agent[S,A] = this
}

/**
  *
  * @param agent
  * @param stateMap
  * @param actionMap
  * @tparam S1
  * @tparam A1
  * @tparam S2
  * @tparam A2
  */
class MappedAgent[S1, A1, S2, A2](
  agent: Agent[S1, A1],
  stateMap: S2 => S1, actionMap: A1 => A2
) extends Agent[S2, A2] {

  def act(state: S2): A2 = actionMap(agent.act(stateMap(state)))

  override def percept(reward: Double): Unit = agent.percept(reward)

  override def clone(): MappedAgent[S1,A1,S2,A2] = new MappedAgent(agent.clone(), stateMap, actionMap)
}

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
