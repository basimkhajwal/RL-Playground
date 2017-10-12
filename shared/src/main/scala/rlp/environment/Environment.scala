package rlp.environment

trait Environment[S] {
  def reset(seed: Int = (math.random()*10000000).toInt): Unit
  def setState(state: S): Unit
  def getState(): S
  def step(): Unit
}

object Environment {
  val DELTA = 1.0/30
}

trait Agent[S, A] {
  def act(state: S): A
  def percept(prevState: S, newState: S, reward: Double): Unit = {}
}
