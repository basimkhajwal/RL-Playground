package rlp.environment

trait Environment[S] {
  def reset(seed: Int = 0): Unit
  def getState(): S
  def step(): Unit
}

object Environment {
  val DELTA = 1.0/30
}

trait Agent[S, A, E <: Environment[S]] {
  def act(state: S): A
  def percept(prevState: S, newState: S, reward: Double): Unit
}
