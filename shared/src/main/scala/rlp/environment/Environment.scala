package rlp.environment

trait Environment[S] {
  def reset(seed: Int = (math.random()*10000000).toInt): Unit
  def setState(state: S): Unit
  def getState(): S
  def step(): Boolean
}

object Environment {
  val DELTA = 1.0/30
}

trait Agent[S, A] {
  def act(state: S): A
  def percept(reward: Double): Unit = {}
  override def clone():Agent[S,A] = this
}

class MappedAgent[S1, A1, S2, A2](
  agent: Agent[S1, A1],
  stateMap: S2 => S1, actionMap: A1 => A2
) extends Agent[S2, A2] {

  def act(state: S2): A2 = actionMap(agent.act(stateMap(state)))

  override def percept(reward: Double): Unit = agent.percept(reward)

  override def clone(): MappedAgent[S1,A1,S2,A2] = new MappedAgent(agent.clone(), stateMap, actionMap)
}

trait SARSAAgent[S, A] extends Agent[S, A] {

  private var lastReward: Double = 0
  private var lastAction: A = _
  private var lastState: S = _

  override def act(state: S): A = {
    lastAction = sarsa(lastState, lastAction, lastReward, state)
    lastState = state
    lastAction
  }

  override def percept(reward: Double): Unit = {
    lastReward = reward
  }

  def sarsa(prevState: S, action: A, reward: Double, newState: S): A

  override def clone(): SARSAAgent[S, A] = sarsa
}
