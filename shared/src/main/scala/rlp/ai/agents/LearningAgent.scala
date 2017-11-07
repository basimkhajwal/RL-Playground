package rlp.ai.agents

import rlp.environment.Agent

sealed trait Space

case class BoxedSpace(low: Double, high: Double, divisions: Int = 10) extends Space {
  require(low < high, "The bound for a boxed space must be positive!")
}

case class DiscreteSpace(n: Int) extends Space {
  require(n > 0, "Discrete space size must be positive")
}

trait LearningAgent extends Agent[Array[Double], Int]{

  def initialise(spaces: Space*): Unit = {}
}

