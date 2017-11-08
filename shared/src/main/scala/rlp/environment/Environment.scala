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
}
