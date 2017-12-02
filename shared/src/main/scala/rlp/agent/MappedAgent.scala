package rlp.agent

/**
  * Utility wrapper to handle mapping
  * from Agent[S1,A1] -> Agent[S2,A2]
  * given functions mapping the percept states and action states
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
  stateMap: S2 => S1,
  actionMap: A1 => A2
) extends Agent[S2, A2] {

  def act(state: S2): A2 = actionMap(agent.act(stateMap(state)))

  override def percept(reward: Double, done: Boolean): Unit = agent.percept(reward, done)

  override def duplicate(): Agent[S2, A2] = new MappedAgent(agent.duplicate(), stateMap, actionMap)

  override def clone(): MappedAgent[S1,A1,S2,A2] = new MappedAgent(agent.clone(), stateMap, actionMap)

  override def reset(): Unit = agent.reset()
}

